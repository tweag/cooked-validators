{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cooked.MockChain.Balancing
  ( balancedTxSkel,
    balancedTx,
    txSkelInputData,
  )
where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.Shelley.API as CardanoLedger
import qualified Cardano.Node.Emulator as Emulator
import Control.Arrow
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Default
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger.Index as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx.Numeric as Pl

balancedTxSkel :: MonadBlockChainBalancing m => TxSkel -> m (TxSkel, Fee, Set PV2.TxOutRef)
balancedTxSkel skelUnbal = do
  let balancingWallet =
        case txOptBalanceWallet . txSkelOpts $ skelUnbal of
          BalanceWithFirstSigner -> case txSkelSigners skelUnbal of
            [] -> error "Can't select balancing wallet: There has to be at least one wallet in txSkelSigners"
            bw : _ -> bw
          BalanceWith bWallet -> bWallet
  let collateralWallet = balancingWallet
  (skel, fee) <-
    if txOptBalance . txSkelOpts $ skelUnbal
      then
        setFeeAndBalance
          balancingWallet
          skelUnbal
      else return (skelUnbal, Fee 0)
  collateralInputs <- calcCollateral collateralWallet -- TODO: Why is it OK to balance first and then add collateral?
  return (skel, fee, collateralInputs)

-- | Take the output of 'balancedTxSkel' and turn it into an actual Cardano
-- transaction.
balancedTx :: MonadBlockChainBalancing m => (TxSkel, Fee, Set PV2.TxOutRef) -> m (C.Tx C.BabbageEra)
balancedTx (skel, fee, collateralInputs) = do
  params <- applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skel) <$> getParams
  consumedData <- txSkelInputData skel
  consumedOrReferencedTxOuts <- do
    ins <- txSkelInputUtxosPV2 skel
    insRef <- txSkelReferenceInputUtxosPV2 skel
    insCollateral <-
      Map.fromList
        <$> mapM
          ( \oref -> do
              mTxOut <- txOutByRef oref
              case mTxOut of
                Nothing -> throwError $ OtherMockChainError "unkown collateral input TxOutRef"
                Just txOut -> return (oref, txOut)
          )
          (Set.toList collateralInputs)
    return $ ins <> insRef <> insCollateral
  consumedValidators <- txSkelInputValidators skel
  case generateTx def {gtpCollateralIns = collateralInputs, gtpFee = fee} params consumedData consumedOrReferencedTxOuts consumedValidators skel of
    Left err -> throwError . MCEGenerationError $ err
    Right tx -> return tx

-- | Ensure that the transaction outputs have the necessary minimum amount of
-- Ada on them. This will only be applied if the 'txOptEnsureMinAda' is set to
-- @True@.
ensureTxSkelOutsMinAda :: MonadBlockChainBalancing m => TxSkel -> m TxSkel
ensureTxSkelOutsMinAda skel = do
  theParams <- applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skel) <$> getParams
  case mapM (ensureTxSkelOutHasMinAda theParams) $ skel ^. txSkelOutsL of
    Left err -> throwError $ MCEGenerationError err
    Right newTxSkelOuts -> return $ skel & txSkelOutsL .~ newTxSkelOuts
  where
    ensureTxSkelOutHasMinAda :: Emulator.Params -> TxSkelOut -> Either GenerateTxError TxSkelOut
    ensureTxSkelOutHasMinAda theParams txSkelOut@(Pays output) = do
      cardanoTxOut <- txSkelOutToCardanoTxOut theParams txSkelOut
      let Pl.Lovelace oldAda = output ^. outputValueL % adaL
          CardanoLedger.Coin requiredAda =
            CardanoLedger.evaluateMinLovelaceOutput (Emulator.emulatorPParams theParams)
              . C.toShelleyTxOut C.ShelleyBasedEraBabbage
              . C.toCtxUTxOTxOut
              $ cardanoTxOut
          updatedTxSkelOut = Pays $ output & outputValueL % adaL .~ Pl.Lovelace (max oldAda requiredAda)
      -- The following iterative approach to calculate the minimum Ada amount
      -- of a TxOut is necessary, because the additional value might make the
      -- TxOut heavier.
      --
      -- It is inspired by
      -- https://github.com/input-output-hk/plutus-apps/blob/8706e6c7c525b4973a7b6d2ed7c9d0ef9cd4ef46/plutus-ledger/src/Ledger/Index.hs#L124
      if oldAda < requiredAda
        then ensureTxSkelOutHasMinAda theParams updatedTxSkelOut
        else return txSkelOut

txSkelInputUtxosPV2 :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.TxOutRef PV2.TxOut)
txSkelInputUtxosPV2 = lookupUtxosPV2 . Map.keys . txSkelIns

txSkelInputUtxos :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.TxOutRef Ledger.TxOut)
txSkelInputUtxos = lookupUtxos . Map.keys . txSkelIns

txSkelReferenceInputUtxosPV2 :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.TxOutRef PV2.TxOut)
txSkelReferenceInputUtxosPV2 = lookupUtxosPV2 . Set.toList . txSkelInsReference

txSkelReferenceInputUtxos :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.TxOutRef Ledger.TxOut)
txSkelReferenceInputUtxos = lookupUtxos . Set.toList . txSkelInsReference

-- | All validators which protect transaction inputs
txSkelInputValidators :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.ValidatorHash (Pl.Versioned PV2.Validator))
txSkelInputValidators skel = do
  utxos <- Map.toList <$> lookupUtxosPV2 (Map.keys . txSkelIns $ skel)
  mValidators <-
    mapM
      ( \(_oref, out) -> case outputAddress out of
          PV2.Address (PV2.ScriptCredential valHash) _ -> do
            mVal <- validatorFromHash valHash
            case mVal of
              Nothing ->
                throwError $
                  MCEUnknownValidator
                    "txSkelInputValidators: unkown validator hash on transaction input"
                    valHash
              Just val -> return $ Just (valHash, val)
          _ -> return Nothing
      )
      utxos
  return . Map.fromList . catMaybes $ mValidators

-- Go through all of the 'PV2.TxOutRef's in the list and look them up in the
-- state of the blockchain. If any 'PV2.TxOutRef' can't be resolved, throw an
-- error.
lookupUtxosPV2 :: MonadBlockChainBalancing m => [PV2.TxOutRef] -> m (Map PV2.TxOutRef PV2.TxOut)
lookupUtxosPV2 outRefs = Map.map txOutV2FromLedger <$> lookupUtxos outRefs

lookupUtxos :: MonadBlockChainBalancing m => [PV2.TxOutRef] -> m (Map PV2.TxOutRef Ledger.TxOut)
lookupUtxos outRefs = do
  Map.fromList
    <$> mapM
      ( \oRef -> do
          mOut <- txOutByRefLedger oRef
          out <- case mOut of
            Nothing ->
              throwError $
                MCEUnknownOutRefError
                  "lookupUtxosPV2: unknown TxOutRef"
                  oRef
            Just out -> return out
          return (oRef, out)
      )
      outRefs

-- | look up the UTxOs the transaction consumes, and sum the value contained in
-- them.
txSkelInputValue :: MonadBlockChainBalancing m => TxSkel -> m PV2.Value
txSkelInputValue skel = do
  txSkelInputs <- txSkelInputUtxos skel
  return $ foldMap (PV2.txOutValue . txOutV2FromLedger) txSkelInputs

-- | Look up the data on UTxOs the transaction consumes.
txSkelInputData :: MonadBlockChainBalancing m => TxSkel -> m (Map PV2.DatumHash PV2.Datum)
txSkelInputData skel = do
  txSkelInputs <- Map.elems <$> txSkelInputUtxosPV2 skel
  mDatums <-
    mapM
      ( \output ->
          case output ^. outputDatumL of
            PV2.NoOutputDatum -> return Nothing
            PV2.OutputDatum datum ->
              let dHash = Pl.datumHash datum
               in Just . (dHash,) <$> datumFromHashWithError dHash
            PV2.OutputDatumHash dHash ->
              Just . (dHash,) <$> datumFromHashWithError dHash
      )
      txSkelInputs
  return . Map.fromList . catMaybes $ mDatums
  where
    datumFromHashWithError :: MonadBlockChainBalancing m => Pl.DatumHash -> m PV2.Datum
    datumFromHashWithError dHash = do
      mDatum <- datumFromHash dHash
      case mDatum of
        Nothing ->
          throwError $
            MCEUnknownDatum
              "txSkelInputData: Transaction input with un-resolvable datum hash"
              dHash
        Just datum -> return datum

-- ensuring that the equation
--
-- > value in inputs + minted value = value in outputs + burned value + fee
--
-- holds. The fee depends on the transaction size, which might change during the
-- process of balancing, because additional inputs belonging to the 'balancePK'
-- might be added to ensure that transaction inputs can cover all of the
-- outputs. This means that fee calculation and balancing are tied together. We
-- follow /plutus-apps/ in breaking this mutual dependency with a fixpoint
-- iteration, which should compute realistic fees.
--
--  This function also adjusts the transaction outputs to contain at least the
--  minimum Ada amount, if the 'txOptEnsureMinAda option is @True@.
setFeeAndBalance :: MonadBlockChainBalancing m => Wallet -> TxSkel -> m (TxSkel, Fee)
setFeeAndBalance balanceWallet skel0 = do
  -- do the min Ada adjustment if it's requested
  skel <-
    if txOptEnsureMinAda . txSkelOpts $ skel0
      then ensureTxSkelOutsMinAda skel0
      else return skel0
  -- all UTxOs belonging to the balancing public key
  balancePKUtxos <- Map.fromList <$> utxosAtLedger (walletAddress balanceWallet)
  -- all UTxOs that the txSkel consumes.
  txSkelUtxos <- txSkelInputUtxos skel
  -- all UTxOs that the txSkel references.
  txSkelReferencedUtxos <- txSkelReferenceInputUtxos skel
  case Ledger.fromPlutusIndex $ Ledger.UtxoIndex $ txSkelReferencedUtxos <> txSkelUtxos <> balancePKUtxos of
    Left err -> throwError $ FailWith $ "setFeeAndValidRange: " ++ show err
    Right cUtxoIndex -> do
      -- We start with a high startingFee, but theres a chance that 'w' doesn't have enough funds
      -- so we'll see an unbalanceable error; in that case, we switch to the minimum fee and try again.
      -- That feels very much like a hack, and it is. Maybe we should witch to starting with a small
      -- fee and then increasing, but that might require more iterations until its settled.
      -- For now, let's keep it just like the folks from plutus-apps did it.
      let startingFee = Fee 3000000
      calcFee 5 startingFee cUtxoIndex skel
        `catchError` \case
          -- Impossible to balance the transaction
          MCEUnbalanceable _ _ ->
            -- WARN
            -- "Pl.minFee" takes an actual Tx but we no longer provide it
            -- since we work on "TxSkel". However, for now, the
            -- implementation of "Pl.minFee" is a constant of 10 lovelace.
            -- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Index.hs#L116
            let minFee = Fee 10 -- forall tx. Pl.minFee tx = 10 lovelace
             in calcFee 5 minFee cUtxoIndex skel
          -- Impossible to generate the Cardano transaction at all
          e -> throwError e
  where
    -- Inspired by https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-contract/src/Wallet/Emulator/Wallet.hs#L329

    calcFee ::
      MonadBlockChainBalancing m =>
      Int ->
      Fee ->
      Emulator.UTxO Emulator.EmulatorEra ->
      TxSkel ->
      m (TxSkel, Fee)
    calcFee n fee cUtxoIndex skel = do
      attemptedSkel <- balanceTxFromAux balanceWallet skel fee
      managedData <- txSkelInputData skel
      managedTxOuts <- do
        ins <- txSkelInputUtxosPV2 skel
        insRef <- txSkelReferenceInputUtxosPV2 skel
        return $ ins <> insRef
      managedValidators <- txSkelInputValidators skel
      theParams <- applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skel) <$> getParams
      case estimateTxSkelFee theParams cUtxoIndex managedData managedTxOuts managedValidators attemptedSkel fee of
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
              calcFee (n - 1) newFee cUtxoIndex skel

-- | This funcion is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee ::
  Emulator.Params ->
  Emulator.UTxO Emulator.EmulatorEra ->
  Map Pl.DatumHash Pl.Datum ->
  Map PV2.TxOutRef PV2.TxOut ->
  Map PV2.ValidatorHash (Pl.Versioned PV2.Validator) ->
  TxSkel ->
  Fee ->
  Either MockChainError Fee
estimateTxSkelFee params cUtxoIndex managedData managedTxOuts managedValidators skel fee = do
  txBodyContent <-
    left MCEGenerationError $
      generateTxBodyContent def {gtpFee = fee} params managedData managedTxOuts managedValidators skel
  let nkeys = C.estimateTransactionKeyWitnessCount txBodyContent
  txBody <-
    left
      ( \case
          Left err -> MCEValidationError err
          Right err -> MCEGenerationError (ToCardanoError "makeTransactionBody" err)
      )
      $ Emulator.makeTransactionBody params cUtxoIndex (Ledger.CardanoBuildTx txBodyContent)
  case C.evaluateTransactionFee (Emulator.pProtocolParams params) txBody nkeys 0 of
    C.Lovelace fee -> pure $ Fee fee

-- | Calculates the collateral for a transaction
calcCollateral :: MonadBlockChainBalancing m => Wallet -> m (Set PV2.TxOutRef)
calcCollateral w = do
  souts <-
    runUtxoSearch $
      utxosAtSearch (walletAddress w)
        `filterWithPure` isOutputWithoutDatum
        `filterWithPure` isOnlyAdaOutput
  when (null souts) $
    throwError MCENoSuitableCollateral
  -- TODO We only keep one element of the list because we are limited on
  -- how many collateral inputs a transaction can have. Should this be
  -- investigated further for a better approach?
  return $ Set.fromList $ take 1 (fst <$> souts)

balanceTxFromAux :: MonadBlockChainBalancing m => Wallet -> TxSkel -> Fee -> m TxSkel
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
    valueAndRefs :: [(PV2.TxOutRef, PV2.TxOut)] -> (PV2.Value, [PV2.TxOutRef])
    valueAndRefs x = (mconcat (outputValue . snd <$> x), fst <$> x)

data BalanceTxRes = BalanceTxRes
  { -- | Inputs that need to be added in order to cover the value in the
    -- transaction outputs
    newInputs :: [(PV2.TxOutRef, PV2.TxOut)],
    -- | The 'newInputs' will add _at least_ the missing value to cover the
    -- outputs, this is the difference of the input value together with the
    -- 'newInputs' and the output value.  This value must be nonnegative in
    -- every asset class.
    returnValue :: PV2.Value,
    -- | Some additional UTxOs that could be used as extra inputs. These all
    -- belong to the same wallet that was passed to 'calcBalanceTx' as an
    -- argument, and are sorted in decreasing order of their Ada value.
    availableUtxos :: [(PV2.TxOutRef, PV2.TxOut)]
  }
  deriving (Show)

-- | Calculate the changes needed to balance a transaction with money from a
-- given wallet.  Every transaction that is sent to the chain must be balanced,
-- that is: @inputs + mints == outputs + fee + burns@.
calcBalanceTx :: MonadBlockChainBalancing m => Wallet -> TxSkel -> Fee -> m BalanceTxRes
calcBalanceTx balanceWallet skel fee = do
  inValue <- (<> positivePart (txSkelMintsValue $ txSkelMints skel)) <$> txSkelInputValue skel -- transaction inputs + minted value
  let outValue = txSkelOutputValue skel fee -- transaction outputs + fee + burned value
      difference = outValue <> Pl.negate inValue
      -- This is the value that must still be paid by 'balancePK' in order to
      -- balance the transaction:
      missingValue = positivePart difference
      -- This will be paid to 'balancePK' in any case:
      initialExcess = negativePart difference
      -- All TxOutRefs that the transaction consumes. We'll need them to make sure
      -- that no additional UTxOs are chosen that are in fact already present on the
      -- transaction.
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
    sortBy (flip compare `on` Pl.fromValue . outputValue . snd)
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
      [(PV2.TxOutRef, PV2.TxOut)] ->
      [(PV2.TxOutRef, PV2.TxOut)] ->
      PV2.Value ->
      PV2.Value ->
      Maybe BalanceTxRes
    selectNewInputs available chosen excess missing =
      case view flattenValueI missing of
        [] -> Just $ BalanceTxRes chosen excess available
        (ac, _) : _ ->
          -- Find the first UTxO belonging to the wallet that contains at least
          -- one token of the required asset class (The hope is that it'll
          -- contain at least @n@ such tokens, but we can't yet fail if there are
          -- fewer; we might need to add several UTxOs):
          case findIndex ((`Pl.geq` Pl.assetClassValue ac 1) . outputValue . snd) available of
            Nothing -> Nothing -- The wallet owns nothing of the required asset class. We can't balance with this wallet.
            Just i ->
              let (left, theChosenUtxo : right) = splitAt i available
                  available' = left ++ right
                  chosen' = theChosenUtxo : chosen
                  theChosenValue = outputValue $ snd theChosenUtxo
                  theChosenDifference = missing <> Pl.negate theChosenValue
                  excess' = excess <> negativePart theChosenDifference
                  missing' = positivePart theChosenDifference
               in -- A remark on why the following line should not lead to an
                  -- infinite recursion: The value described by @missing'@ is
                  -- strictly smaller than the value described by @missing@,
                  -- because there was at least one token of the asset class @ac@
                  -- in @theChosenValue@.
                  selectNewInputs available' chosen' excess' missing'

-- | Once we calculated what is needed to balance a transaction @skel@, we still
-- need to apply those changes to @skel@. Because of the 'Pl.minAdaTxOut'
-- constraint, this might not be possible: imagine the leftover is less than
-- 'Pl.minAdaTxOut', but the transaction has no output addressed to the
-- balancing wallet. If we just create a new ouput for the balancing wallet and
-- place the leftover there, the resulting transaction will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another
-- UTxO belonging to the wallet to then create the output with the proper
-- leftover. If the wallet has no UTxO, then there's no way to balance this
-- transaction.
applyBalanceTx :: PV2.PubKeyHash -> BalanceTxRes -> TxSkel -> Maybe TxSkel
applyBalanceTx balancePK (BalanceTxRes newInputs returnValue availableUtxos) skel = do
  -- Here we'll try a few things, in order, until one of them succeeds:
  --
  -- 1. If allowed by the balanceOutputPolicy, pick out the best possible output
  --    to adjust and adjust it as long as it remains with more than
  --    'Pl.minAdaTxOut'. No need for additional inputs apart from the
  --    @newInputs@. The "best possible" here means the most valuable ada-only
  --    output without any datum that will be paid to the given wallet. If the
  --    policy doesn't allow modifying an existing utxo or no such utxo exists,
  --    we move on to the next option;
  --
  -- 2. If the leftover is more than 'Pl.minAdaTxOut' and (1) wasn't possible,
  --    create a new output to return leftover. No need for additional inputs
  --    besides the @newInputs@.
  --
  -- 3. Attempt to consume other possible utxos from 'w' in order to combine
  --    them and return the leftover.

  -- TODO: Mustn't every UTxO belonging to the wallet contain at least minAda?
  -- In that case, we could forget about adding several additional inputs. If
  -- one isn't enough, there's nothing we can do, no?
  let -- All transaction outputs
      outs :: [TxSkelOut]
      outs = txSkelOuts skel

      -- All transaction outputs that are ada-only, datum-free, and belong to
      -- 'balancePK', together with the index of the corresponding 'TxSkelOut'
      -- in 'outs'.
      candidateOutputsWithIndices :: [(Int, ConcreteOutput PV2.PubKeyHash () Pl.Ada PV2.ScriptHash)]
      candidateOutputsWithIndices =
        mapMaybe
          ( \(i, Pays output) ->
              case ( isPKOutputFrom balancePK
                       >=> isOnlyAdaOutput
                       >=> isOutputWithoutDatum
                       >=> Just . toOutputWithReferenceScriptHash
                   )
                output of
                Nothing -> Nothing
                Just output' -> Just (fromIntegral i, output')
          )
          $ zip [0 :: Int ..] outs

      -- The index of the "best possible transaction output", as described
      -- above.
      mBestOutputIndex :: Maybe Int
      mBestOutputIndex = case sortBy
        ( flip compare
            `on` (^. outputValueL) -- This lens use means something different than 'outputValue', because it returns 'Pl.Ada'!
              . snd
        )
        candidateOutputsWithIndices of
        [] -> Nothing
        (best, _) : _ -> Just best

      ins = txSkelIns skel
  (newIns, newOuts) <-
    case mBestOutputIndex of
      Just i ->
        let (left, bestTxOut : right) = splitAt i outs
         in case txOptBalanceOutputPolicy $ txSkelOpts skel of
              AdjustExistingOutput ->
                let bestTxOutValue = txSkelOutValue bestTxOut
                    adjustedValue = bestTxOutValue <> returnValue
                 in if adjustedValue `Pl.geq` Pl.toValue Ledger.minAdaTxOutEstimated -- TODO make thid dependen on the atual TxOut
                      then
                        Just -- (1)
                          ( ins <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs),
                            left ++ (bestTxOut & txSkelOutValueL .~ adjustedValue) : right
                          )
                      else tryAdditionalInputs ins outs availableUtxos returnValue
              DontAdjustExistingOutput -> tryAdditionalOutput ins outs
      Nothing ->
        -- There's no "best possible transaction output" in the sense described
        -- above.
        tryAdditionalOutput ins outs
  return skel {txSkelIns = newIns, txSkelOuts = newOuts}
  where
    tryAdditionalOutput ::
      Map PV2.TxOutRef TxSkelRedeemer ->
      [TxSkelOut] ->
      Maybe (Map PV2.TxOutRef TxSkelRedeemer, [TxSkelOut])
    tryAdditionalOutput ins outs =
      if Pl.fromValue returnValue >= Ledger.minAdaTxOutEstimated -- TODO make thid dependen on the atual TxOut
        then
          Just -- (2)
            ( ins <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs),
              outs ++ [paysPK balancePK returnValue]
            )
        else tryAdditionalInputs ins outs availableUtxos returnValue

    tryAdditionalInputs ::
      Map PV2.TxOutRef TxSkelRedeemer ->
      [TxSkelOut] ->
      [(PV2.TxOutRef, PV2.TxOut)] ->
      Pl.Value ->
      Maybe (Map PV2.TxOutRef TxSkelRedeemer, [TxSkelOut])
    tryAdditionalInputs ins outs available return =
      case available of
        [] -> Nothing
        (newTxOutRef, newTxOut) : newAvailable ->
          let additionalValue = outputValue newTxOut
              newReturn = additionalValue <> return
              newIns =
                ( ins
                    <> Map.fromSet (const TxSkelNoRedeemerForPK) (Set.fromList $ map fst newInputs)
                    <> Map.singleton newTxOutRef TxSkelNoRedeemerForPK
                )
              newOuts = outs ++ [paysPK balancePK newReturn]
           in if newReturn `Pl.geq` Pl.toValue Ledger.minAdaTxOutEstimated -- TODO make thid dependen on the atual TxOut
                then Just (newIns, newOuts) -- (3)
                else tryAdditionalInputs newIns newOuts newAvailable newReturn
