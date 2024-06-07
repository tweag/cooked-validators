-- | This module handles auto-balancing of transaction skeleton. This includes
-- computation of fees and collaterals because their computation cannot be
-- separated from the balancing.
module Cooked.MockChain.Balancing (balanceTxSkel, calcMaxFee) where

import Cardano.Api.Ledger qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Emulator
import Control.Monad.Except
import Cooked.Conversion
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.MinAda
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ratio qualified as Rat
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Index qualified as Ledger
import Optics.Core hiding (chosen)
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- | This is the main entry point of our balancing mechanism. This function
-- takes a skeleton and makes an attempt at balancing it using existing utxos
-- from the balancing wallet. Note that the input skeleton might, or might not,
-- be properly adjusted with minimal ada in existing utxo. The balancing
-- mechanism will not attempt to modify existing paiement, but will ensure
-- additional payments satisfy the min ada constraint. This balancing only
-- occurs when requested in the skeleton options.
balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef, Wallet)
balanceTxSkel skelUnbal = do
  -- We retrieve the balancing wallet, who is central in the balancing
  -- process. Any missing asset will be searched within its utxos.
  balancingWallet <- case txOptBalanceWallet . txSkelOpts $ skelUnbal of
    BalanceWithFirstSigner -> case txSkelSigners skelUnbal of
      [] -> fail "Can't select balancing wallet: There has to be at least one wallet in txSkelSigners"
      bw : _ -> return bw
    BalanceWith bWallet -> return bWallet

  -- Initial fees is the largest possible fee. When the balancing is not
  -- required, this fee will be applied. Otherwise, a dychotomic search will
  -- happen between this fee and 0 until an optimal fee is found.
  initialFee <- calcMaxFee

  -- We collect collateral inputs. They might be directly provided in the
  -- skeleton, or should be retrieved from a given wallet
  (collateralIns, returnCollateralWallet) <- case txOptCollateralUtxos . txSkelOpts $ skelUnbal of
    CollateralUtxosFromBalancingWallet -> (,balancingWallet) . Set.fromList . map fst <$> runUtxoSearch (vanillaOutputsAtSearch balancingWallet)
    CollateralUtxosFromWallet cWallet -> (,cWallet) . Set.fromList . map fst <$> runUtxoSearch (vanillaOutputsAtSearch cWallet)
    CollateralUtxosFromSet utxos rWallet -> return (utxos, rWallet)

  -- We either return the original skeleton with default fees and associated
  -- collaterals when no balancing is requested. Or, we return the balanced
  -- skeleton with adjusted fees and collaterals, which are computed with a
  -- maximum of 5 balancing iterations (empirically sufficient).
  (txSkelBal, fee, adjustedCollateralIns) <-
    if txOptBalance . txSkelOpts $ skelUnbal
      then calcFee balancingWallet (Fee 0) initialFee collateralIns returnCollateralWallet skelUnbal
      else return (skelUnbal, initialFee, collateralIns)

  return (txSkelBal, fee, adjustedCollateralIns, returnCollateralWallet)

-- | This computes the maximum possible fee a transaction can cost based on the
-- current protocol parameters
calcMaxFee :: (MonadBlockChainBalancing m) => m Fee
calcMaxFee = do
  -- Default parameters in case they are not present. It is unclear when/if this
  -- could actually happen though.
  let defMaxTxExecutionUnits =
        Cardano.ExecutionUnits {executionSteps = 10_000_000_000, executionMemory = 14_000_000}
      defExecutionUnitPrices =
        Cardano.ExecutionUnitPrices {priceExecutionSteps = 721 Rat.% 10_000_000, priceExecutionMemory = 577 Rat.% 10_000}
  -- Parameters necessary to compute the maximum possible fee for a transaction
  params <- Emulator.pProtocolParams <$> getParams
  let maxTxSize = toInteger $ Cardano.protocolParamMaxTxSize params
      Emulator.Coin txFeePerByte = Cardano.protocolParamTxFeePerByte params
      Emulator.Coin txFeeFixed = Cardano.protocolParamTxFeeFixed params
      Cardano.ExecutionUnitPrices priceESteps priceEMem = fromMaybe defExecutionUnitPrices $ Cardano.protocolParamPrices params
      Cardano.ExecutionUnits (toInteger -> eSteps) (toInteger -> eMem) = fromMaybe defMaxTxExecutionUnits $ Cardano.protocolParamMaxTxExUnits params
  -- Intermediate computations
  let sizeFees = txFeeFixed + (maxTxSize * txFeePerByte)
      eStepsFees = (eSteps * Rat.numerator priceESteps) `div` Rat.denominator priceESteps
      eMemFees = (eMem * Rat.numerator priceEMem) `div` Rat.denominator priceEMem
  -- Final fee accounts for the size of the transaction and the units consumed
  -- by the execution of scripts from the transaction
  return $ Fee $ sizeFees + eStepsFees + eMemFees

-- | Balances a skeleton and computes optimal fees using a dychotomic search
calcFee :: (MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> Set Api.TxOutRef -> Wallet -> TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef)
calcFee _ minFee maxFee _ _ _ | minFee >= maxFee = fail "cannot balance"
calcFee balanceWallet minFee@(Fee a) maxFee@(Fee b) collateralIns returnCollateralWallet skel | fee <- Fee $ div (a + b) 2 = do
  attemptedSkel <- balanceTxFromAux balanceWallet skel fee
  adjustedCollateralIns <- collateralInsFromFees fee collateralIns returnCollateralWallet
  newFee <- estimateTxSkelFee attemptedSkel fee adjustedCollateralIns returnCollateralWallet
  case newFee - fee of
    x | x == 0 || (x < 0 && (b - a) == 1) -> return (attemptedSkel, newFee, adjustedCollateralIns)
    x | x < 0 -> calcFee balanceWallet minFee fee collateralIns returnCollateralWallet skel
    _ -> calcFee balanceWallet (fee + 1) maxFee collateralIns returnCollateralWallet skel

-- | This reduces a set of given collateral inputs while accounting for:
-- * the percentage to respect between fees and total collaterals
-- * min ada in the associated return collateral
-- * maximum number of collateral inputs
collateralInsFromFees :: (MonadBlockChainBalancing m) => Fee -> Set Api.TxOutRef -> Wallet -> m (Set Api.TxOutRef)
collateralInsFromFees fee collateralIns (paysPK -> paysToColWallet) = do
  params <- getParams
  -- We retrieve the number max of collateral inputs, with a default of 10. In
  -- practice this will be around 3.
  nbMax <- toInteger . fromMaybe 10 . Cardano.protocolParamMaxCollateralInputs . Emulator.pProtocolParams <$> getParams
  -- We retrieve the percentage to respect between fees and total collaterals
  percentage <- toInteger . fromMaybe 100 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams <$> getParams
  -- We compute the total collateral to be associated to the transaction as a
  -- value. This will be the target value to be reached by collateral inputs.
  let totalCollateral = toValue . Cardano.Coin . (`div` 100) . (* percentage) . feeLovelace $ fee
  -- Collateral tx outputs sorted by decreased ada amount
  collateralTxOuts <- runUtxoSearch (txOutByRefSearch $ Set.toList collateralIns)
  -- Candidate subsets of utxos to be used as collaterals
  let candidatesRaw = reachValue collateralTxOuts totalCollateral nbMax
  -- Decorated candidates with min ada and actual ada
  let candidatesDecorated = second (\val -> (Script.fromValue val, getTxSkelOutMinAda params $ paysToColWallet val)) <$> candidatesRaw
  -- Filtered candidates that have successfully been generated and have enough
  -- ada to be considered as valid return collateral payments
  let candidatesFiltered = [(fst <$> l, lv) | (l, (Script.Lovelace lv, Right minLv)) <- candidatesDecorated, minLv <= lv]
  -- We return the most cost efficient candidate
  case sortBy (compare `on` snd) candidatesFiltered of
    [] -> throwError MCENoSuitableCollateral
    (txOutRefs, _) : _ -> return $ Set.fromList txOutRefs

reachValue :: [(Api.TxOutRef, Api.TxOut)] -> Api.Value -> Integer -> [([(Api.TxOutRef, Api.TxOut)], Api.Value)]
-- Target is smaller than the empty value (which means in only contains negative
-- entries), we stop looking as adding more elements would be superfluous.
reachValue _ target _ | target `Api.leq` mempty = [([], PlutusTx.negate target)]
-- The target is not reached, but the max number of elements is reached, we
-- would need more elements but are not allowed to looked for them.
reachValue _ _ maxEls | maxEls == 0 = []
-- The target is not reached, and cannot possibly be reached, as the remaining
-- candidates do not sum up to the target.
reachValue l target _ | not $ target `Api.leq` mconcat (Api.txOutValue . snd <$> l) = []
-- There is no more elements to go through and the target has not been
-- reached. Encompassed in the previous case, but required by GHC which cannot
-- know the function is total without this case.
reachValue [] _ _ = []
-- Main recursive case, where we get to either pick or drop the first element
reachValue (h@(_, Api.txOutValue -> hVal) : t) target maxEls =
  (++)
    -- dropping the first element
    (reachValue t target maxEls)
    -- taking the first element
    (first (h :) <$> reachValue t (target <> PlutusTx.negate hVal) (maxEls - 1))

-- | This function is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: (MonadBlockChainBalancing m) => TxSkel -> Fee -> Set Api.TxOutRef -> Wallet -> m Fee
estimateTxSkelFee skel fee collateralIns returnCollateralWallet = do
  params <- getParams
  managedData <- txSkelInputData skel
  managedTxOuts <- do
    ins <- txSkelInputUtxosPl skel
    insRef <- txSkelReferenceInputUtxosPl skel
    collateralUtxos <- lookupUtxosPl (Set.toList collateralIns)
    return $ ins <> insRef <> collateralUtxos
  managedValidators <- txSkelInputValidators skel
  txBodyContent <- case generateBodyContent fee returnCollateralWallet collateralIns params managedData managedTxOuts managedValidators skel of
    Left err -> throwError $ MCEGenerationError err
    Right txBodyContent -> return txBodyContent
  let nkeys = Cardano.estimateTransactionKeyWitnessCount txBodyContent
      pParams = Emulator.pEmulatorPParams params
  case Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent of
    Left err -> throwError $ MCEGenerationError (TxBodyError "Error creating body when estimating fees" err)
    Right txBody | Emulator.Coin fee' <- Cardano.evaluateTransactionFee Cardano.ShelleyBasedEraConway pParams txBody nkeys 0 -> return $ Fee fee'

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
    Nothing -> throwError $ MCEUnbalanceable (MCEUnbalNotEnoughFunds balanceWallet missingValue) skel
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
          Just output'
            | Script.Lovelace amount <- output' ^. outputValueL ->
                if amount < maybe 0 (\(_, (_, x), _) -> x) currentBest
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
