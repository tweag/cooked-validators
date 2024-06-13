-- | This module handles auto-balancing of transaction skeleton. This includes
-- computation of fees and collaterals because their computation cannot be
-- separated from the balancing.
module Cooked.MockChain.Balancing (balanceTxSkel, getMinAndMaxFee, estimateTxSkelFee) where

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
import Data.Map qualified as Map
import Data.Maybe
import Data.Ratio qualified as Rat
import Data.Set (Set)
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- * A few types to make the functions in this module more readable

type Fee = Integer

type Collaterals = Set Api.TxOutRef

type BalancingOutputs = [(Api.TxOutRef, Api.TxOut)]

-- | This is the main entry point of our balancing mechanism. This function
-- takes a skeleton and makes an attempt at balancing it using existing utxos
-- from the balancing wallet. Note that the input skeleton might, or might not,
-- be properly adjusted with minimal ada in existing utxo. The balancing
-- mechanism will not attempt to modify existing paiement, but will ensure
-- additional payments satisfy the min ada constraint. This balancing only
-- occurs when requested in the skeleton options. This function returns the
-- balanced skeleton, the associated fee, associated collateral utxos and return
-- collateral wallet.
balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Collaterals, Wallet)
balanceTxSkel skelUnbal@TxSkel {..} = do
  -- We retrieve the possible balancing wallet. Any extra payment will be
  -- redirected to them, and utxos will be taken from their wallet if associated
  -- with the BalancingUtxosAutomatic policy
  balancingWallet <- case txOptBalancingPolicy txSkelOpts of
    BalanceWithFirstSigner -> case txSkelSigners of
      [] -> fail "Can't select balancing wallet from the signers lists because it is empty."
      bw : _ -> return $ Just bw
    BalanceWith bWallet -> return $ Just bWallet
    DoNotBalance -> return Nothing

  -- Initial fees is the largest possible fee. When the balancing is not
  -- required, this fee will be applied. Otherwise, a dychotomic search will
  -- happen between this fee and 0 until an optimal fee is found.
  (minFee, maxFee) <- getMinAndMaxFee

  -- We collect collateral inputs. They might be directly provided in the
  -- skeleton, or should be retrieved from a given wallet
  (collateralIns, returnCollateralWallet) <- case txOptCollateralUtxos txSkelOpts of
    CollateralUtxosFromBalancingWallet -> case balancingWallet of
      Nothing -> fail "Can't select collateral utxos from a balancing wallet because it does not exist."
      Just bWallet -> (,bWallet) . Set.fromList . map fst <$> runUtxoSearch (onlyValueOutputsAtSearch bWallet)
    CollateralUtxosFromWallet cWallet -> (,cWallet) . Set.fromList . map fst <$> runUtxoSearch (onlyValueOutputsAtSearch cWallet)
    CollateralUtxosFromSet utxos rWallet -> return (utxos, rWallet)

  (txSkelBal, fee, adjustedCollateralIns) <- case balancingWallet of
    Nothing ->
      -- The balancing should not be performed. We still adjust the collaterals
      -- though around a provided fee, or the maximum fee.
      let fee = case txOptFeePolicy txSkelOpts of
            AutoFeeComputation -> maxFee
            ManualFee fee' -> fee'
       in (skelUnbal,fee,) <$> collateralInsFromFees fee collateralIns returnCollateralWallet
    Just bWallet -> do
      -- We collect the balancing utxos based on the associated options. We filter
      -- out utxos already used in the input skeleton.
      (filter ((`notElem` txSkelKnownTxOutRefs skelUnbal) . fst) -> balancingUtxos) <-
        runUtxoSearch $ case txOptBalancingUtxos txSkelOpts of
          BalancingUtxosAutomatic -> onlyValueOutputsAtSearch bWallet `filterWithAlways` outputTxOut
          BalancingUtxosWith utxos -> txOutByRefSearch (Set.toList utxos) `filterWithPure` isPKOutput `filterWithAlways` outputTxOut
      case txOptFeePolicy txSkelOpts of
        -- If fee are left for us to compute, we run a dichotomic search. This
        -- is full auto mode, the most powerful but time-consuming.
        AutoFeeComputation ->
          computeFeeAndBalance bWallet minFee maxFee collateralIns balancingUtxos returnCollateralWallet skelUnbal
        -- If fee are provided manually, we adjust the collaterals and the
        -- skeleton around them.
        ManualFee fee -> do
          adjustedCollateralIns <- collateralInsFromFees fee collateralIns returnCollateralWallet
          attemptedSkel <- computeBalancedTxSkel bWallet balancingUtxos skelUnbal fee
          return (attemptedSkel, fee, adjustedCollateralIns)

  return (txSkelBal, fee, adjustedCollateralIns, returnCollateralWallet)

-- | This computes the minimum and maximum possible fee a transaction can cost
-- based on the current protocol parameters
getMinAndMaxFee :: (MonadBlockChainBalancing m) => m (Fee, Fee)
getMinAndMaxFee = do
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
  -- Final fee accounts for the size of the transaction and the units consumed
  -- by the execution of scripts from the transaction
  let sizeFees = txFeeFixed + (maxTxSize * txFeePerByte)
      eStepsFees = (eSteps * Rat.numerator priceESteps) `div` Rat.denominator priceESteps
      eMemFees = (eMem * Rat.numerator priceEMem) `div` Rat.denominator priceEMem
  return (txFeeFixed, sizeFees + eStepsFees + eMemFees)

attemptBalancingAndCollaterals :: (MonadBlockChainBalancing m) => Wallet -> Collaterals -> BalancingOutputs -> Wallet -> Fee -> TxSkel -> m (Collaterals, TxSkel)
attemptBalancingAndCollaterals balancingWallet collateralIns balancingUtxos returnCollateralWallet fee skel = do
  adjustedCollateralIns <- collateralInsFromFees fee collateralIns returnCollateralWallet
  attemptedSkel <- computeBalancedTxSkel balancingWallet balancingUtxos skel fee
  return (adjustedCollateralIns, attemptedSkel)

-- | Balances a skeleton and computes optimal fees using a dychotomic search
-- over a given interval of fee. The lower bound is excluded while the upper
-- bound is included in the interval.
computeFeeAndBalance :: (MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> Collaterals -> BalancingOutputs -> Wallet -> TxSkel -> m (TxSkel, Fee, Collaterals)
computeFeeAndBalance _ minFee maxFee _ _ _ _
  | minFee > maxFee =
      throwError $ FailWith "Unreachable case, please report a bug at https://github.com/tweag/cooked-validators/issues"
computeFeeAndBalance balancingWallet minFee maxFee collateralIns balancingUtxos returnCollateralWallet skel
  | minFee == maxFee = do
      (adjustedCollateralIns, attemptedSkel) <- attemptBalancingAndCollaterals balancingWallet collateralIns balancingUtxos returnCollateralWallet minFee skel
      return (attemptedSkel, minFee, adjustedCollateralIns)
computeFeeAndBalance balancingWallet minFee maxFee collateralIns balancingUtxos returnCollateralWallet skel
  | fee <- div (minFee + maxFee) 2 = do
      -- We attempt to compte a balanced skeleton and associated collateral. If one
      -- of the two fails but the interval is not unitary, there is a chance this
      -- can still succeed for smaller fee. Otherwise, we just spread the error.
      attemptedBalancing <-
        fmap Just (attemptBalancingAndCollaterals balancingWallet collateralIns balancingUtxos returnCollateralWallet fee skel)
          `catchError` \case
            MCEUnbalanceable {} | fee - minFee > 1 -> return Nothing
            MCENoSuitableCollateral {} | fee - minFee > 1 -> return Nothing
            err -> throwError err

      case attemptedBalancing of
        -- The skeleton was not balanceable, we try strictly smaller fee
        Nothing -> computeFeeAndBalance balancingWallet minFee (fee - 1) collateralIns balancingUtxos returnCollateralWallet skel
        -- The skeleton was balanceable, we compute and analyse the resulting
        -- fee to seach upwards or downwards for an optimal solution
        Just (adjustedCollateralIns, attemptedSkel) -> do
          newFee <- estimateTxSkelFee attemptedSkel fee adjustedCollateralIns returnCollateralWallet
          let (newMinFee, newMaxFee) = if newFee <= fee then (minFee, fee) else (fee + 1, maxFee)
          computeFeeAndBalance balancingWallet newMinFee newMaxFee collateralIns balancingUtxos returnCollateralWallet skel

-- | This reduces a set of given collateral inputs while accounting for:
-- * the percentage to respect between fees and total collaterals
-- * min ada in the associated return collateral
-- * maximum number of collateral inputs
collateralInsFromFees :: (MonadBlockChainBalancing m) => Fee -> Collaterals -> Wallet -> m Collaterals
collateralInsFromFees fee collateralIns returnCollateralWallet = do
  -- We retrieve the number max of collateral inputs, with a default of 10. In
  -- practice this will be around 3.
  nbMax <- toInteger . fromMaybe 10 . Cardano.protocolParamMaxCollateralInputs . Emulator.pProtocolParams <$> getParams
  -- We retrieve the percentage to respect between fees and total collaterals
  percentage <- toInteger . fromMaybe 100 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams <$> getParams
  -- We compute the total collateral to be associated to the transaction as a
  -- value. This will be the target value to be reached by collateral inputs.
  let totalCollateral = toValue . Cardano.Coin . (`div` 100) . (* percentage) $ fee
  -- Collateral tx outputs sorted by decreased ada amount
  collateralTxOuts <- runUtxoSearch (txOutByRefSearch $ Set.toList collateralIns)
  -- Candidate subsets of utxos to be used as collaterals
  let candidatesRaw = reachValue collateralTxOuts totalCollateral nbMax
  -- Preparing a possible collateral error
  let noSuitableCollateralError = MCENoSuitableCollateral fee percentage totalCollateral
  -- Retrieving and returning the best candidate as a utxo set
  Set.fromList . fst <$> getOptimalCandidate candidatesRaw returnCollateralWallet noSuitableCollateralError

-- | The main computing function for optimal balancing and collaterals. It
-- computes the subsets of a set of UTxOs that sum up to a certain target. It
-- stops when the target is reached, not adding superfluous UTxOs. Despite
-- optimizations, this function is theoretically in 2^n where n is the number of
-- candidate UTxOs. Use with caution.
reachValue :: BalancingOutputs -> Api.Value -> Integer -> [(BalancingOutputs, Api.Value)]
-- Target is smaller than the empty value (which means in only contains negative
-- entries), we stop looking as adding more elements would be superfluous.
reachValue _ target _ | target `Api.leq` mempty = [([], PlutusTx.negate target)]
-- The target is not reached, but the max number of elements is reached, we
-- would need more elements but are not allowed to look for them.
reachValue _ _ maxEls | maxEls == 0 = []
-- The target is not reached, and cannot possibly be reached, as the remaining
-- candidates do not sum up to the target.
reachValue l target _ | not $ target `Api.leq` mconcat (Api.txOutValue . snd <$> l) = []
-- There is no more elements to go through and the target has not been
-- reached. Encompassed by the previous case, but needed by GHC.
reachValue [] _ _ = []
-- Main recursive case, where we either pick or drop the head. We only pick the
-- head if it contributes to reaching the target, i.e. if its intersection with
-- the positive part of the target is not empty.
reachValue (h@(_, Api.txOutValue -> hVal) : t) target maxEls =
  (++) (reachValue t target maxEls) $
    if snd (Api.split target) PlutusTx./\ hVal == mempty
      then []
      else first (h :) <$> reachValue t (target <> PlutusTx.negate hVal) (maxEls - 1)

-- | A helper function to grab an optimal candidate in terms of having a minimal
-- enough amount of ada to sustain itself meant to be used after calling
-- `reachValue`. This throws an error when there are no suitable candidates.
getOptimalCandidate :: (MonadBlockChainBalancing m) => [(BalancingOutputs, Api.Value)] -> Wallet -> MockChainError -> m ([Api.TxOutRef], Api.Value)
getOptimalCandidate candidates paymentTarget mceError = do
  params <- getParams
  -- We decorate the candidates with their current ada and min ada requirements
  let candidatesDecorated = second (\val -> (val, Script.fromValue val, getTxSkelOutMinAda params $ paysPK paymentTarget val)) <$> candidates
      -- We filter the candidates that have enough ada to sustain themselves
      candidatesFiltered = [(minLv, (fst <$> l, val)) | (l, (val, Script.Lovelace lv, Right minLv)) <- candidatesDecorated, minLv <= lv]
  case sortBy (compare `on` fst) candidatesFiltered of
    -- If the list of candidates is empty, we throw an error
    [] -> throwError mceError
    (_, ret) : _ -> return ret

-- | This function is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: (MonadBlockChainBalancing m) => TxSkel -> Fee -> Collaterals -> Wallet -> m Fee
estimateTxSkelFee skel fee collateralIns returnCollateralWallet = do
  -- We retrieve the necessary data to generate the transaction body
  params <- getParams
  managedData <- txSkelInputData skel
  managedTxOuts <- lookupUtxosPl $ txSkelKnownTxOutRefs skel <> Set.toList collateralIns
  managedValidators <- txSkelInputValidators skel
  -- We generate the transaction body content, handling errors in the meantime
  txBodyContent <- case generateBodyContent fee returnCollateralWallet collateralIns params managedData managedTxOuts managedValidators skel of
    Left err -> throwError $ MCEGenerationError err
    Right txBodyContent -> return txBodyContent
  -- We create the actual body and send if for validation
  txBody <- case Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent of
    Left err -> throwError $ MCEGenerationError (TxBodyError "Error creating body when estimating fees" err)
    Right txBody -> return txBody
  -- We retrieve the estimate number of required witness in the transaction
  let nkeys = Cardano.estimateTransactionKeyWitnessCount txBodyContent
  -- We return an accurate estimate of the resulting transaction fee
  return $ Emulator.unCoin $ Cardano.evaluateTransactionFee Cardano.ShelleyBasedEraConway (Emulator.pEmulatorPParams params) txBody nkeys 0

-- | This creates a balanced skeleton from a given skeleton and fee
-- In other words, this ensures that the following equation holds:
-- input value + minted value = output value + burned value + fee
computeBalancedTxSkel :: (MonadBlockChainBalancing m) => Wallet -> BalancingOutputs -> TxSkel -> Fee -> m TxSkel
computeBalancedTxSkel balancingWallet balancingUtxos txSkel@TxSkel {..} (lovelace -> feeValue) = do
  -- We compute the necessary values from the skeleton that are part of the
  -- equation, except for the `feeValue` which we already have.
  let (burnedValue, mintedValue) = Api.split $ txSkelMintsValue txSkelMints
      outValue = foldOf (txSkelOutsL % folded % txSkelOutValueL) txSkel
  inValue <- txSkelInputValue txSkel
  -- We compute the values missing in the left and right side of the equation
  let (missingRight, missingLeft) = Api.split $ outValue <> burnedValue <> feeValue <> PlutusTx.negate (inValue <> mintedValue)
  -- This gives us what we need to run our `reachValue` algorithm and append to
  -- the resulting values whatever payment was missing in the initial skeleton
  let candidatesRaw = second (<> missingRight) <$> reachValue balancingUtxos missingLeft (toInteger $ length balancingUtxos)
  -- We prepare a possible balancing error with the difference between the
  -- requested amount and the maximum amount provided by the balancing wallet
  let totalValue = mconcat $ Api.txOutValue . snd <$> balancingUtxos
      difference = snd $ Api.split $ missingLeft <> PlutusTx.negate totalValue
      balancingError = MCEUnbalanceable balancingWallet difference txSkel
  -- Which one of our candidates should be picked depends on three factors
  -- - Whether there exists a perfect candidate set with empty surplus value
  -- - The `BalancingOutputPolicy` in the skeleton options
  -- - The presence of an existing output at the balancing wallet address
  (additionalInsTxOutRefs, newTxSkelOuts) <- case find ((== mempty) . snd) candidatesRaw of
    -- There exists a perfect candidate, this is the rarest and easiest
    -- scenario, as the outputs will not change due to balancing. This means
    -- that there was no missing value on the right and the balancing utxos
    -- exactly account for what was missing on the left.
    Just (txOutRefs, _) -> return (fst <$> txOutRefs, txSkelOuts)
    -- There in an existing output at the owner's address and the balancing
    -- policy allows us to adjust it with additional value.
    Nothing
      | (before, txSkelOut : after) <- break (\(Pays o) -> toCredential (o ^. outputOwnerL) == toCredential balancingWallet) txSkelOuts,
        AdjustExistingOutput <- txOptBalanceOutputPolicy txSkelOpts -> do
          -- We get the optimal candidate based on an updated value. We update
          -- the `txSkelOuts` by replacing the value content of the selected
          -- output. We keep intact the orders of those outputs.
          let candidatesRaw' = second (<> txSkelOut ^. txSkelOutValueL) <$> candidatesRaw
          (txOutRefs, val) <- getOptimalCandidate candidatesRaw' balancingWallet balancingError
          return (txOutRefs, before ++ (txSkelOut & txSkelOutValueL .~ val) : after)
    -- There is no output at the balancing wallet address, or the balancing
    -- policy forces us to create a new output, both yielding the same result.
    _ -> do
      -- We get the optimal candidate, and update the `txSkelOuts` by appending
      -- a new output at the end of the list, to keep the order intact.
      (txOutRefs, val) <- getOptimalCandidate candidatesRaw balancingWallet balancingError
      return (txOutRefs, txSkelOuts ++ [paysPK balancingWallet val])
  let newTxSkelIns = txSkelIns <> Map.fromList ((,TxSkelNoRedeemerForPK) <$> additionalInsTxOutRefs)
  return $ (txSkel & txSkelOutsL .~ newTxSkelOuts) & txSkelInsL .~ newTxSkelIns
