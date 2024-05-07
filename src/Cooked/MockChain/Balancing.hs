-- | This module handles auto-balancing of transaction skeleton. This includes
-- computation of fees and collaterals as well as ensuring outputs have the
-- required minimum ada. Most of the balancing process can be customized through
-- `TxSkelOpts`
module Cooked.MockChain.Balancing (balanceTxSkel) where

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.Core qualified as Shelley
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Emulator
import Control.Arrow
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Index qualified as Ledger
import Optics.Core hiding (chosen)
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef)
balanceTxSkel skelUnbal = do
  -- We retrieve the balancing wallet, who is central in the balancing
  -- process. Any missing asset will be searched within its utxos.
  let balancingWallet =
        case txOptBalanceWallet . txSkelOpts $ skelUnbal of
          BalanceWithFirstSigner -> case txSkelSigners skelUnbal of
            [] -> error "Can't select balancing wallet: There has to be at least one wallet in txSkelSigners"
            bw : _ -> bw
          BalanceWith bWallet -> bWallet

  -- We collect collateral inputs. They might be directly provided in the
  -- skeleton, or should be retrieved from a given wallet
  collateralInputs <- case txOptCollateralUtxos . txSkelOpts $ skelUnbal of
    CollateralUtxosFromBalancingWallet -> getCollateralInputs balancingWallet
    CollateralUtxosFromWallet cWallet -> getCollateralInputs cWallet
    CollateralUtxosFromSet utxos -> return utxos

  -- We compute the balanced skeleton with the associated fees
  (skel, fees) <-
    if txOptBalance . txSkelOpts $ skelUnbal
      then setFeeAndBalance balancingWallet skelUnbal collateralInputs
      else return (skelUnbal, Fee 0)

  -- We return the new skeleton, the fees and the collateral inputs
  return (skel, fees, collateralInputs)

-- | Ensure that the transaction outputs have the necessary minimum amount of
-- Ada on them. This will only be applied if the 'txOptEnsureMinAda' is set to
-- @True@.
ensureTxSkelOutsMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
ensureTxSkelOutsMinAda skel = do
  theParams <- getParams
  case mapM (ensureTxSkelOutHasMinAda theParams) $ skel ^. txSkelOutsL of
    Left err -> throwError $ MCEGenerationError err
    Right newTxSkelOuts -> return $ skel & txSkelOutsL .~ newTxSkelOuts
  where
    ensureTxSkelOutHasMinAda :: Emulator.Params -> TxSkelOut -> Either GenerateTxError TxSkelOut
    ensureTxSkelOutHasMinAda theParams txSkelOut@(Pays output) = do
      cardanoTxOut <- generateTxOut (Emulator.pNetworkId theParams) txSkelOut
      let Script.Lovelace oldAda = output ^. outputValueL % adaL
          Emulator.Coin requiredAda =
            Shelley.getMinCoinTxOut
              (Emulator.emulatorPParams theParams)
              . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
              . Cardano.toCtxUTxOTxOut
              $ cardanoTxOut
          updatedTxSkelOut = Pays $ output & outputValueL % adaL .~ Script.Lovelace (max oldAda requiredAda)
      -- The following iterative approach to calculate the minimum Ada amount of
      -- a TxOut is necessary, because the additional value might make the TxOut
      -- heavier.
      --
      -- It is inspired by
      -- https://github.com/input-output-hk/plutus-apps/blob/8706e6c7c525b4973a7b6d2ed7c9d0ef9cd4ef46/plutus-ledger/src/Ledger/Index.hs#L124
      if oldAda < requiredAda
        then ensureTxSkelOutHasMinAda theParams updatedTxSkelOut
        else return txSkelOut

-- ensuring that the equation
--
-- > input value + minted value = output value + burned value + fee
--
-- holds. The fee depends on the transaction size, which might change during the
-- process of balancing, because additional inputs belonging to the 'balancePK'
-- might be added to ensure that transaction inputs can cover all of the
-- outputs. This means that fee calculation and balancing are tied together. We
-- follow /plutus-apps/ in breaking this mutual dependency with a fixpoint
-- iteration, which should compute realistic fees.
--
--  This function also adjusts the transaction outputs to contain at least the
--  minimum Ada amount, if the 'txOptEnsureMinAda is @True@.
setFeeAndBalance :: (MonadBlockChainBalancing m) => Wallet -> TxSkel -> Set Api.TxOutRef -> m (TxSkel, Fee)
setFeeAndBalance balanceWallet skel0 collateralIns = do
  -- do the min Ada adjustment if it's requested
  skel <-
    if txOptEnsureMinAda . txSkelOpts $ skel0
      then ensureTxSkelOutsMinAda skel0
      else return skel0

  -- all UTxOs used as collaterals
  collateralUtxosPl <- lookupUtxosPl (Set.toList collateralIns)

  -- We start with a high startingFee, but theres a chance that 'w' doesn't have
  -- enough funds so we'll see an unbalanceable error; in that case, we switch
  -- to the minimum fee and try again.  That feels very much like a hack, and it
  -- is. Maybe we should witch to starting with a small fee and then increasing,
  -- but that might require more iterations until its settled.  For now, let's
  -- keep it just like the folks from plutus-apps did it.
  calcFee 5 (Fee 3_000_000) skel collateralUtxosPl
    `catchError` \case
      -- Impossible to balance the transaction
      MCEUnbalanceable _ _ ->
        -- WARN "Api.minFee" takes an actual Tx but we no longer provide it
        -- since we work on "TxSkel". However, for now, the implementation of
        -- "Api.minFee" is a constant of 10 lovelace.
        -- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Index.hs#L116
        -- forall tx. Api.minFee tx = 10 lovelace
        calcFee 5 (Fee 10) skel collateralUtxosPl
      -- Impossible to generate the Cardano transaction at all
      e -> throwError e
  where
    -- Inspired by https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-contract/src/Wallet/Emulator/Wallet.hs#L329

    calcFee ::
      (MonadBlockChainBalancing m) =>
      Int ->
      Fee ->
      TxSkel ->
      Map Api.TxOutRef Api.TxOut ->
      m (TxSkel, Fee)
    calcFee n fee skel collateralUtxos = do
      attemptedSkel <- balanceTxFromAux balanceWallet skel fee
      managedData <- txSkelInputData skel
      managedTxOuts <- do
        ins <- txSkelInputUtxosPl skel
        insRef <- txSkelReferenceInputUtxosPl skel
        return $ ins <> insRef <> collateralUtxos
      managedValidators <- txSkelInputValidators skel
      theParams <- applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skel) <$> getParams
      case estimateTxSkelFee theParams managedData managedTxOuts managedValidators attemptedSkel fee collateralIns of
        -- necessary to capture script failure for failed cases
        Left err@MCEValidationError {} -> throwError err
        Left err -> throwError $ MCECalcFee err
        Right newFee
          | newFee == fee -> do
              -- Debug.Trace.traceM "Reached fixpoint:"
              -- Debug.Trace.traceM $ "- fee = " <> show fee
              -- Debug.Trace.traceM $ "- skeleton = " <> show (attemptedSkel {_txSkelFee = fee})
              pure (attemptedSkel, fee) -- reached fixpoint
          | n == 0 -> do
              -- Debug.Trace.traceM $ "Max iteration reached: newFee = " <> show newFee
              pure (attemptedSkel, max newFee fee) -- maximum number of iterations
          | otherwise -> do
              -- Debug.Trace.traceM $ "New iteration: newfee = " <> show newFee
              calcFee (n - 1) newFee skel collateralUtxos

-- | This funcion is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee ::
  Emulator.Params ->
  Map Api.DatumHash Api.Datum ->
  Map Api.TxOutRef Api.TxOut ->
  Map Script.ValidatorHash (Script.Versioned Script.Validator) ->
  TxSkel ->
  Fee ->
  Set Api.TxOutRef ->
  Either MockChainError Fee
estimateTxSkelFee params managedData managedTxOuts managedValidators skel fees collateralIns = do
  txBodyContent <-
    left MCEGenerationError $
      generateBodyContent fees collateralIns params managedData managedTxOuts managedValidators skel
  let nkeys = Cardano.estimateTransactionKeyWitnessCount txBodyContent
  txBody <-
    left (MCEGenerationError . TxBodyError "Error creating body when estimating fees") $
      Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent
  case Cardano.evaluateTransactionFee Cardano.ShelleyBasedEraConway (Emulator.pEmulatorPParams params) txBody nkeys 0 of
    Emulator.Coin fee -> pure $ Fee fee

-- TODO: improve our collateral mechanism

-- | Calculates the collateral for a transaction
getCollateralInputs :: (MonadBlockChainBalancing m) => Wallet -> m (Set Api.TxOutRef)
getCollateralInputs w = do
  souts <-
    runUtxoSearch $
      utxosAtSearch (walletAddress w)
        `filterWithPure` isOutputWithoutDatum
        `filterWithPure` isOnlyAdaOutput
  case souts of
    [] -> throwError MCENoSuitableCollateral
    -- TODO We only keep one element of the list because we are limited on how
    -- many collateral inputs a transaction can have. Should this be
    -- investigated further for a better approach?
    ((txOutRef, _) : _) -> return $ Set.singleton txOutRef

balanceTxFromAux :: (MonadBlockChainBalancing m) => Wallet -> TxSkel -> Fee -> m TxSkel
balanceTxFromAux balanceWallet txskel fee = do
  bres@(BalanceTxRes {newInputs, returnValue, availableUtxos}) <- calcBalanceTx balanceWallet txskel fee
  case applyBalanceTx (walletPKHash balanceWallet) bres txskel of
    Just txskel' -> return txskel'
    Nothing ->
      throwError $
        MCEUnbalanceable
          ( MCEUnbalNotEnoughReturning
              (valueAndRefs newInputs)
              (valueAndRefs availableUtxos)
              returnValue
          )
          txskel
  where
    valueAndRefs :: [(Api.TxOutRef, Api.TxOut)] -> (Api.Value, [Api.TxOutRef])
    valueAndRefs x = (mconcat (outputValue . snd <$> x), fst <$> x)

data BalanceTxRes = BalanceTxRes
  { -- | Inputs that need to be added in order to cover the value in the
    -- transaction outputs
    newInputs :: [(Api.TxOutRef, Api.TxOut)],
    -- | The 'newInputs' will add _at least_ the missing value to cover the
    -- outputs, this is the difference of the input value together with the
    -- 'newInputs' and the output value.  This value must be nonnegative in
    -- every asset class.
    returnValue :: Api.Value,
    -- | Some additional UTxOs that could be used as extra inputs. These all
    -- belong to the same wallet that was passed to 'calcBalanceTx' as an
    -- argument, and are sorted in decreasing order of their Ada value.
    availableUtxos :: [(Api.TxOutRef, Api.TxOut)]
  }
  deriving (Show)

-- | Calculate the changes needed to balance a transaction with assets from a
-- given wallet. Every transaction that is sent to the chain must be balanced:
-- @inputs + mints == outputs + fee + burns@.
calcBalanceTx :: (MonadBlockChainBalancing m) => Wallet -> TxSkel -> Fee -> m BalanceTxRes
calcBalanceTx balanceWallet skel fee = do
  inValue <- (<> positivePart (txSkelMintsValue $ txSkelMints skel)) <$> txSkelInputValue skel -- transaction inputs + minted value
  let outValue = txSkelOutputValue skel fee -- transaction outputs + fee + burned value
      difference = outValue <> PlutusTx.negate inValue
      -- This is the value that must still be paid by 'balancePK' in order to
      -- balance the transaction:
      missingValue = positivePart difference
      -- This will be paid to 'balancePK' in any case:
      initialExcess = negativePart difference
      -- All TxOutRefs that the transaction consumes. We'll need them to make
      -- sure that no additional UTxOs are chosen that are in fact already
      -- present on the transaction.
      inputOrefs = Map.keys $ txSkelIns skel
  -- Get all UTxOs that belong to the given wallet, and that are not yet being
  -- consumed on the transaction.
  --
  -- These UTxOs are sorted in decreasing order of their Ada value, which will
  -- make 'selectNewInputs' will more likely select additional inputs that
  -- contain a lot of Ada. The hope behind this heuristic is that it'll
  -- therefore become less likely for the 'returnValue' to be less than the
  -- minimum Ada amount required for each output. See this comment for context:
  -- https://github.com/tweag/cooked-validators/issues/71#issuecomment-1016406041
  candidateUtxos <-
    sortBy (flip compare `on` Script.fromValue . outputValue . snd)
      . filter ((`notElem` inputOrefs) . fst)
      <$> utxosAt (walletAddress balanceWallet)
  case selectNewInputs candidateUtxos [] initialExcess missingValue of
    Nothing ->
      throwError $
        MCEUnbalanceable
          (MCEUnbalNotEnoughFunds balanceWallet missingValue)
          skel
    Just bTxRes -> return bTxRes
  where
    selectNewInputs ::
      [(Api.TxOutRef, Api.TxOut)] ->
      [(Api.TxOutRef, Api.TxOut)] ->
      Api.Value ->
      Api.Value ->
      Maybe BalanceTxRes
    selectNewInputs available chosen excess missing =
      case view flattenValueI missing of
        [] -> Just $ BalanceTxRes chosen excess available
        (ac, _) : _ ->
          -- Find the first UTxO belonging to the wallet that contains at least
          -- one token of the required asset class (The hope is that it'll
          -- contain at least @n@ such tokens, but we can't yet fail if there
          -- are fewer; we might need to add several UTxOs):
          case break ((`Script.geq` Script.assetClassValue ac 1) . outputValue . snd) available of
            (_, []) -> Nothing -- The wallet owns nothing of the required asset class. We can't balance with this wallet.
            (previousUtxos, theChosenUtxo : nextUtxos) ->
              let available' = previousUtxos ++ nextUtxos
                  chosen' = theChosenUtxo : chosen
                  theChosenValue = outputValue $ snd theChosenUtxo
                  theChosenDifference = missing <> PlutusTx.negate theChosenValue
                  excess' = excess <> negativePart theChosenDifference
                  missing' = positivePart theChosenDifference
               in -- A remark on why the following line should not lead to an
                  -- infinite recursion: The value described by @missing'@ is
                  -- strictly smaller than the value described by @missing@,
                  -- because there was at least one token of the asset class
                  -- @ac@ in @theChosenValue@.
                  selectNewInputs available' chosen' excess' missing'

-- | Once we calculated what is needed to balance a transaction @skel@, we still
-- need to apply those changes to @skel@. Because of the 'Script.minAdaTxOut'
-- constraint, this might not be possible: imagine the leftover is less than
-- 'Script.minAdaTxOut', but the transaction has no output addressed to the
-- balancing wallet. If we just create a new ouput for the balancing wallet and
-- place the leftover there, the resulting transaction will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another
-- UTxO belonging to the wallet to then create the output with the proper
-- leftover. If the wallet has no UTxO, then there's no way to balance this
-- transaction.
applyBalanceTx :: Api.PubKeyHash -> BalanceTxRes -> TxSkel -> Maybe TxSkel
applyBalanceTx balancePK (BalanceTxRes newInputs returnValue availableUtxos) skel@TxSkel {..} = do
  -- Here we'll try a few things, in order, until one of them succeeds:
  --
  -- 1. If allowed by the balanceOutputPolicy, pick out the best possible output
  --    to adjust and adjust it as long as it remains with more than
  --    'Script.minAdaTxOut'. No need for additional inputs apart from the
  --    @newInputs@. The "best possible" here means the most valuable ada-only
  --    output without any datum that will be paid to the given wallet. If the
  --    policy doesn't allow modifying an existing utxo or no such utxo exists,
  --    we move on to the next option;
  --
  -- 2. If the leftover is more than 'Script.minAdaTxOut' and (1) wasn't
  --    possible, create a new output to return leftover. No need for additional
  --    inputs besides the @newInputs@.
  --
  -- 3. Attempt to consume other possible utxos from 'w' in order to combine
  --    them and return the leftover.

  -- TODO: Mustn't every UTxO belonging to the wallet contain at least minAda?
  -- In that case, we could forget about adding several additional inputs. If
  -- one isn't enough, there's nothing we can do, no?
  let bestOuts ::
        -- \| Current best partition of the outputs
        Maybe ([TxSkelOut], (TxSkelOut, Integer), [TxSkelOut]) ->
        -- \| Outputs that have already been checked
        [TxSkelOut] ->
        -- \| Outputs that remain to be checked
        [TxSkelOut] ->
        -- \| Returns the best (if any) candidate output alongside its
        -- predecessors and successors
        Maybe ([TxSkelOut], (TxSkelOut, Integer), [TxSkelOut])
      bestOuts currentBest _ [] = currentBest
      bestOuts currentBest processed (txSkelOut@(Pays output) : nexts) =
        case isPKOutputFrom balancePK output >>= isOutputWithoutDatum >>= isOnlyAdaOutput of
          Nothing -> bestOuts currentBest (processed ++ [txSkelOut]) nexts
          Just output' ->
            let (Script.Lovelace amount) = output' ^. outputValueL
             in if amount < maybe 0 (\(_, (_, x), _) -> x) currentBest
                  then -- This is a good candidate but a better one was found before
                    bestOuts currentBest (processed ++ [txSkelOut]) nexts
                  else -- This is the best candidate so far
                    Just (processed, (txSkelOut, amount), nexts)

  (newIns, newOuts) <-
    case bestOuts Nothing [] txSkelOuts of
      Nothing ->
        -- There's no "best possible transaction output" in the sense described
        -- above.
        tryAdditionalOutput txSkelIns txSkelOuts
      Just (previousUtxos, (bestTxOut, _), nextUtxos) ->
        case txOptBalanceOutputPolicy txSkelOpts of
          AdjustExistingOutput ->
            let bestTxOutValue = txSkelOutValue bestTxOut
                adjustedValue = bestTxOutValue <> returnValue
             in if adjustedValue `Script.geq` Script.toValue Ledger.minAdaTxOutEstimated -- TODO make this depende on the atual TxOut
                  then
                    Just -- (1)
                      ( txSkelIns <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs),
                        previousUtxos ++ (bestTxOut & txSkelOutValueL .~ adjustedValue) : nextUtxos
                      )
                  else tryAdditionalInputs txSkelIns txSkelOuts availableUtxos returnValue
          DontAdjustExistingOutput -> tryAdditionalOutput txSkelIns txSkelOuts
  return skel {txSkelIns = newIns, txSkelOuts = newOuts}
  where
    tryAdditionalOutput ::
      Map Api.TxOutRef TxSkelRedeemer ->
      [TxSkelOut] ->
      Maybe (Map Api.TxOutRef TxSkelRedeemer, [TxSkelOut])
    tryAdditionalOutput ins outs =
      if Script.fromValue returnValue >= Ledger.minAdaTxOutEstimated -- TODO make this depend on the atual TxOut
        then
          Just -- (2)
            ( ins <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs),
              outs ++ [paysPK balancePK returnValue]
            )
        else tryAdditionalInputs ins outs availableUtxos returnValue

    tryAdditionalInputs ::
      Map Api.TxOutRef TxSkelRedeemer ->
      [TxSkelOut] ->
      [(Api.TxOutRef, Api.TxOut)] ->
      Api.Value ->
      Maybe (Map Api.TxOutRef TxSkelRedeemer, [TxSkelOut])
    tryAdditionalInputs ins outs available oldReturn =
      case available of
        [] -> Nothing
        (newTxOutRef, newTxOut) : newAvailable ->
          let additionalValue = outputValue newTxOut
              newReturn = additionalValue <> oldReturn
              newIns =
                ( ins
                    <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs)
                    <> Map.singleton newTxOutRef TxSkelNoRedeemerForPK
                )
              newOuts = outs ++ [paysPK balancePK newReturn]
           in if newReturn `Script.geq` Script.toValue Ledger.minAdaTxOutEstimated -- TODO make this dependen on the actual TxOut
                then Just (newIns, newOuts) -- (3)
                else tryAdditionalInputs newIns newOuts newAvailable newReturn
