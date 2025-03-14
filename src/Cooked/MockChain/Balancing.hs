-- | This module handles auto-balancing of transaction skeleton. This includes
-- computation of fees and collaterals because their computation cannot be
-- separated from the balancing.
module Cooked.MockChain.Balancing
  ( balanceTxSkel,
    getMinAndMaxFee,
    estimateTxSkelFee,
  )
where

import Cardano.Api.Ledger qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Emulator
import Control.Monad
import Control.Monad.Except
import Cooked.Conversion
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.MinAda
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Ratio qualified as Rat
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- * A few types to make the functions in this module more readable

type Fee = Integer

type Collaterals = Set Api.TxOutRef

type BalancingOutputs = [(Api.TxOutRef, Api.TxOut)]

-- | This is the main entry point of our balancing mechanism. This function
-- takes a skeleton and returns a (possibly) balanced skeleton alongside the
-- associated fee, collateral inputs and return collateral wallet, which might
-- be empty when no script is involved in the transaction. The options from the
-- skeleton control whether it should be balanced, and how to compute its
-- associated elements.
balanceTxSkel :: (MonadBlockChainBalancing m) => TxSkel -> m (TxSkel, Fee, Maybe (Collaterals, Wallet))
balanceTxSkel skelUnbal@TxSkel {..} = do
  -- We retrieve the possible balancing wallet. Any extra payment will be
  -- redirected to them, and utxos will be taken from their wallet if associated
  -- with the BalancingUtxosFromBalancingWallet policy
  balancingWallet <- case txOptBalancingPolicy txSkelOpts of
    BalanceWithFirstSigner -> case txSkelSigners of
      [] -> fail "Can't select a balancing wallet from the list of signers because it is empty."
      bw : _ -> return $ Just bw
    BalanceWith bWallet -> return $ Just bWallet
    DoNotBalance -> return Nothing

  -- The protocol parameters indirectly dictate a minimal and maximal value for a
  -- single transaction fee, which we retrieve.
  (minFee, maxFee) <- getMinAndMaxFee

  -- We collect collateral inputs candidates. They might be directly provided in
  -- the skeleton, or should be retrieved from a given wallet. They are
  -- associated with a return collateral wallet, which we retrieve as well. All
  -- of this is wrapped in a `Maybe` type to represent the case when the
  -- transaction does not involve script and should not have any kind of
  -- collaterals attached to it.
  mCollaterals <- do
    -- We retrieve the various kinds of scripts
    spendingScripts <- txSkelInputValidators skelUnbal
    -- The transaction will only require collaterals when involving scripts
    let noScriptInvolved =
          Map.null txSkelMints
            && null (mapMaybe txSkelProposalWitness txSkelProposals)
            && Map.null spendingScripts
            && null (txSkelWithdrawalsScripts skelUnbal)
    case (noScriptInvolved, txOptCollateralUtxos txSkelOpts) of
      (True, CollateralUtxosFromSet utxos _) -> logEvent (MCLogUnusedCollaterals $ Right utxos) >> return Nothing
      (True, CollateralUtxosFromWallet cWallet) -> logEvent (MCLogUnusedCollaterals $ Left cWallet) >> return Nothing
      (True, CollateralUtxosFromBalancingWallet) -> return Nothing
      (False, CollateralUtxosFromSet utxos rWallet) -> return $ Just (utxos, rWallet)
      (False, CollateralUtxosFromWallet cWallet) -> Just . (,cWallet) . Set.fromList . map fst <$> runUtxoSearch (onlyValueOutputsAtSearch cWallet)
      (False, CollateralUtxosFromBalancingWallet) -> case balancingWallet of
        Nothing -> fail "Can't select collateral utxos from a balancing wallet because it does not exist."
        Just bWallet -> Just . (,bWallet) . Set.fromList . map fst <$> runUtxoSearch (onlyValueOutputsAtSearch bWallet)

  -- At this point, the presence (or absence) of balancing wallet dictates
  -- whether the transaction should be automatically balanced or not.
  (txSkelBal, fee, adjustedColsAndWallet) <- case balancingWallet of
    Nothing ->
      -- The balancing should not be performed. We still adjust the collaterals
      -- though around a provided fee, or the maximum fee.
      let fee = case txOptFeePolicy txSkelOpts of
            AutoFeeComputation -> maxFee
            ManualFee fee' -> fee'
       in (skelUnbal,fee,) <$> collateralsFromFees fee mCollaterals
    Just bWallet -> do
      -- The balancing should be performed. We collect the candidates balancing
      -- utxos based on the associated policy
      balancingUtxos <-
        case txOptBalancingUtxos txSkelOpts of
          BalancingUtxosFromBalancingWallet -> runUtxoSearch $ onlyValueOutputsAtSearch bWallet `filterWithAlways` outputTxOut
          BalancingUtxosFromSet utxos ->
            -- We resolve the given set of utxos
            runUtxoSearch (txOutByRefSearch (Set.toList utxos))
              -- We filter out those belonging to scripts, while throwing a
              -- warning if any was actually discarded.
              >>= filterAndWarn (isJust . isPKOutput . snd) "They belong to scripts."
          -- We filter the candidate utxos by removing those already present in the
          -- skeleton, throwing a warning if any was actually discarded
          >>= filterAndWarn ((`notElem` txSkelKnownTxOutRefs skelUnbal) . fst) "They are already used in the skeleton."

      case txOptFeePolicy txSkelOpts of
        -- If fees are left for us to compute, we run a dichotomic search. This
        -- is full auto mode, the most powerful but time-consuming.
        AutoFeeComputation ->
          computeFeeAndBalance bWallet minFee maxFee balancingUtxos mCollaterals skelUnbal
        -- If fee are provided manually, we adjust the collaterals and the
        -- skeleton around them directly.
        ManualFee fee -> do
          adjustedColsAndWallet <- collateralsFromFees fee mCollaterals
          attemptedSkel <- computeBalancedTxSkel bWallet balancingUtxos skelUnbal fee
          return (attemptedSkel, fee, adjustedColsAndWallet)

  return (txSkelBal, fee, adjustedColsAndWallet)
  where
    filterAndWarn f s l
      | (ok, toInteger . length -> koLength) <- partition f l =
          unless (koLength == 0) (logEvent $ MCLogDiscardedUtxos koLength s) >> return ok

-- | This computes the minimum and maximum possible fee a transaction can cost
-- based on the current protocol parameters
getMinAndMaxFee :: (MonadBlockChainBalancing m) => m (Fee, Fee)
getMinAndMaxFee = do
  -- Default parameters in case they are not present. It is unclear when/if this
  -- could actually happen though. These default values have been taken from the
  -- current default instance of the protocol parameters.
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

-- | Computes optimal fee for a given skeleton and balances it around those fees.
-- This uses a dichotomic search for an optimal "balanceable around" fee.
computeFeeAndBalance :: (MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> BalancingOutputs -> Maybe (Collaterals, Wallet) -> TxSkel -> m (TxSkel, Fee, Maybe (Collaterals, Wallet))
computeFeeAndBalance _ minFee maxFee _ _ _
  | minFee > maxFee =
      throwError $ FailWith "Unreachable case, please report a bug at https://github.com/tweag/cooked-validators/issues"
computeFeeAndBalance balancingWallet minFee maxFee balancingUtxos mCollaterals skel
  | minFee == maxFee = do
      -- The fee interval is reduced to a single element, we balance around it
      (adjustedColsAndWallet, attemptedSkel) <- attemptBalancingAndCollaterals balancingWallet balancingUtxos minFee mCollaterals skel
      return (attemptedSkel, minFee, adjustedColsAndWallet)
computeFeeAndBalance balancingWallet minFee maxFee balancingUtxos mCollaterals skel
  | fee <- (minFee + maxFee) `div` 2 = do
      -- The fee interval is larger than a single element. We attempt to balance
      -- around its central point, which can fail due to missing value in
      -- balancing utxos or collateral utxos.
      attemptedBalancing <- catchError
        (Just <$> attemptBalancingAndCollaterals balancingWallet balancingUtxos fee mCollaterals skel)
        $ \case
          -- If it fails, and the remaining fee interval is not reduced to the
          -- current fee attempt, we return `Nothing` which signifies that we
          -- need to keep searching. Otherwise, the whole balancing process
          -- fails and we spread the error.
          MCEUnbalanceable {} | fee - minFee > 0 -> return Nothing
          MCENoSuitableCollateral {} | fee - minFee > 0 -> return Nothing
          err -> throwError err

      (newMinFee, newMaxFee) <- case attemptedBalancing of
        -- The skeleton was not balanceable, we try strictly smaller fee
        Nothing -> return (minFee, fee - 1)
        -- The skeleton was balanceable, we compute and analyse the resulting
        -- fee to seach upwards or downwards for an optimal solution
        Just (adjustedColsAndWallet, attemptedSkel) -> do
          newFee <- estimateTxSkelFee attemptedSkel fee adjustedColsAndWallet
          return $ case fee - newFee of
            -- Current fee is insufficient, we look on the right (strictly)
            n | n < 0 -> (fee + 1, maxFee)
            -- Current fee is sufficient, but the set of balancing utxos cannot
            -- necessarily account for less fee, since it was (magically)
            -- exactly enough to compensate for the missing value. Reducing the
            -- fee would ruin this perfect balancing and force an output to be
            -- created at the balancing wallet address, thus we cannot assume
            -- the actual estimated fee can be accounted for with the current
            -- set of balancing utxos and we cannot speed up search.
            _ | txSkelValueInOutputs attemptedSkel == txSkelValueInOutputs skel -> (minFee, fee)
            -- Current fee is sufficient, and the set of utxo could account for
            -- less fee by feeding into whatever output already goes back to the
            -- balancing wallet. We can speed up search, because the current
            -- attempted skeleton could necessarily account for the estimated
            -- fee of the input skeleton.
            _ -> (minFee, newFee)

      computeFeeAndBalance balancingWallet newMinFee newMaxFee balancingUtxos mCollaterals skel

-- | Helper function to group the two real steps of the balancing: balance a
-- skeleton around a given fee, and compute the associated collateral inputs
attemptBalancingAndCollaterals :: (MonadBlockChainBalancing m) => Wallet -> BalancingOutputs -> Fee -> Maybe (Collaterals, Wallet) -> TxSkel -> m (Maybe (Collaterals, Wallet), TxSkel)
attemptBalancingAndCollaterals balancingWallet balancingUtxos fee mCollaterals skel = do
  adjustedCollateralIns <- collateralsFromFees fee mCollaterals
  attemptedSkel <- computeBalancedTxSkel balancingWallet balancingUtxos skel fee
  return (adjustedCollateralIns, attemptedSkel)

-- | This selects a subset of suitable collateral inputs from a given set while
-- accounting for the ratio to respect between fees and total collaterals, the
-- min ada requirements in the associated return collateral and the maximum
-- number of collateral inputs authorized by protocol parameters.
collateralInsFromFees :: (MonadBlockChainBalancing m) => Fee -> Collaterals -> Wallet -> m Collaterals
collateralInsFromFees fee collateralIns returnCollateralWallet = do
  -- We retrieve the max number of collateral inputs, with a default of 10. In
  -- practice this will be around 3.
  nbMax <- toInteger . fromMaybe 10 . Cardano.protocolParamMaxCollateralInputs . Emulator.pProtocolParams <$> getParams
  -- We retrieve the percentage to respect between fees and total collaterals
  percentage <- toInteger . fromMaybe 100 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams <$> getParams
  -- We compute the total collateral to be associated to the transaction as a
  -- value. This will be the target value to be reached by collateral inputs. We
  -- add one because of ledger requirement which seem to round up this value.
  let totalCollateral = toValue . Cardano.Coin . (+ 1) . (`div` 100) . (* percentage) $ fee
  -- Collateral tx outputs sorted by decreasing ada amount
  collateralTxOuts <- runUtxoSearch (txOutByRefSearch $ Set.toList collateralIns)
  -- Candidate subsets of utxos to be used as collaterals
  let candidatesRaw = reachValue collateralTxOuts totalCollateral nbMax
  -- Preparing a possible collateral error
  let noSuitableCollateralError = MCENoSuitableCollateral fee percentage totalCollateral
  -- Retrieving and returning the best candidate as a utxo set
  Set.fromList . fst <$> getOptimalCandidate candidatesRaw returnCollateralWallet noSuitableCollateralError

-- | This adjusts collateral inputs when necessary
collateralsFromFees :: (MonadBlockChainBalancing m) => Fee -> Maybe (Collaterals, Wallet) -> m (Maybe (Collaterals, Wallet))
collateralsFromFees _ Nothing = return Nothing
collateralsFromFees fee (Just (collateralIns, returnCollateralWallet)) =
  Just . (,returnCollateralWallet) <$> collateralInsFromFees fee collateralIns returnCollateralWallet

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
  let candidatesDecorated = second (\val -> (val, Script.fromValue val, getTxSkelOutMinAda params $ paymentTarget `receives` Value val)) <$> candidates
      -- We filter the candidates that have enough ada to sustain themselves
      candidatesFiltered = [(minLv, (fst <$> l, val)) | (l, (val, Script.Lovelace lv, Right minLv)) <- candidatesDecorated, minLv <= lv]
  case sortBy (compare `on` fst) candidatesFiltered of
    -- If the list of candidates is empty, we throw an error
    [] -> throwError mceError
    (_, ret) : _ -> return ret

-- | This function is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: (MonadBlockChainBalancing m) => TxSkel -> Fee -> Maybe (Collaterals, Wallet) -> m Fee
estimateTxSkelFee skel fee mCollaterals = do
  -- We retrieve the necessary data to generate the transaction body
  params <- getParams
  managedData <- txSkelHashedData skel
  let collateralIns = case mCollaterals of
        Nothing -> []
        Just (s, _) -> Set.toList s
  managedTxOuts <- lookupUtxos $ txSkelKnownTxOutRefs skel <> collateralIns
  managedValidators <- txSkelInputValidators skel
  -- We generate the transaction body content, handling errors in the meantime
  txBodyContent <- case generateBodyContent fee params managedData managedTxOuts managedValidators mCollaterals skel of
    Left err -> throwError $ MCEGenerationError err
    Right txBodyContent -> return txBodyContent
  -- We create the actual body and send if for validation
  txBody <- case Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent of
    Left err -> throwError $ MCEGenerationError $ TxBodyError "Error creating body when estimating fees" err
    Right txBody -> return txBody
  -- We retrieve the estimate number of required witness in the transaction
  let nkeys = Cardano.estimateTransactionKeyWitnessCount txBodyContent
  -- We need to reconstruct an index to pass to the fee estimate function
  -- We begin by retrieving the relevant utxos used in the skeleton
  (knownTxORefs, knownTxOuts) <- unzip . Map.toList <$> lookupUtxos (txSkelKnownTxOutRefs skel <> collateralIns)
  -- We then compute their Cardano counterparts
  let indexOrError = do
        txInL <- forM knownTxORefs Ledger.toCardanoTxIn
        txOutL <- forM knownTxOuts $ Ledger.toCardanoTxOut $ Emulator.pNetworkId params
        return $ Cardano.UTxO $ Map.fromList $ zip txInL $ Cardano.toCtxUTxOTxOut <$> txOutL
  -- We retrieve the index when it was successfully created
  index <- case indexOrError of
    Left err -> throwError $ MCEGenerationError $ ToCardanoError "estimateTxSkelFee: toCardanoError" err
    Right index' -> return index'
  -- We finally can the fee estimate function
  return . Emulator.unCoin $ Cardano.calculateMinTxFee Cardano.ShelleyBasedEraConway (Emulator.pEmulatorPParams params) index txBody nkeys

-- | This creates a balanced skeleton from a given skeleton and fee. In other
-- words, this ensures that the following equation holds: input value + minted
-- value + withdrawn value = output value + burned value + fee + deposits
computeBalancedTxSkel :: (MonadBlockChainBalancing m) => Wallet -> BalancingOutputs -> TxSkel -> Fee -> m TxSkel
computeBalancedTxSkel balancingWallet balancingUtxos txSkel@TxSkel {..} (Script.lovelace -> feeValue) = do
  params <- getParams
  -- We compute the necessary values from the skeleton that are part of the
  -- equation, except for the `feeValue` which we already have.
  let (burnedValue, mintedValue) = Api.split $ txSkelMintsValue txSkelMints
      outValue = txSkelValueInOutputs txSkel
      withdrawnValue = txSkelWithdrawnValue txSkel
  inValue <- txSkelInputValue txSkel
  depositedValue <- toValue <$> txSkelProposalsDeposit txSkel
  -- We compute the values missing in the left and right side of the equation
  let (missingRight, missingLeft) = Api.split $ outValue <> burnedValue <> feeValue <> depositedValue <> PlutusTx.negate (inValue <> mintedValue <> withdrawnValue)
  -- We compute the minimal ada requirement of the missing payment
  rightMinAda <- case getTxSkelOutMinAda params $ balancingWallet `receives` Value missingRight of
    Left err -> throwError $ MCEGenerationError err
    Right a -> return a
  -- We compute the current ada of the missing payment. If the missing payment
  -- is not empty and the minimal ada is not present, some value is missing.
  let Script.Lovelace rightAda = missingRight ^. Script.adaL
      missingAda = rightMinAda - rightAda
      missingAdaValue = if missingRight /= mempty && missingAda > 0 then Script.lovelace missingAda else mempty
  -- The actual missing value on the left might needs to account for any missing
  -- min ada on the missing payment of the transaction skeleton. This also has
  -- to be repercuted on the missing value on the right.
  let missingLeft' = missingLeft <> missingAdaValue
      missingRight' = missingRight <> missingAdaValue
  -- This gives us what we need to run our `reachValue` algorithm and append to
  -- the resulting values whatever payment was missing in the initial skeleton
  let candidatesRaw = second (<> missingRight') <$> reachValue balancingUtxos missingLeft' (toInteger $ length balancingUtxos)
  -- We prepare a possible balancing error with the difference between the
  -- requested amount and the maximum amount provided by the balancing wallet
  let totalValue = mconcat $ Api.txOutValue . snd <$> balancingUtxos
      difference = snd $ Api.split $ missingLeft' <> PlutusTx.negate totalValue
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
          let candidatesRaw' = second (<> txSkelOut ^. (txSkelOutValueL % txSkelOutValueContentL)) <$> candidatesRaw
          (txOutRefs, val) <- getOptimalCandidate candidatesRaw' balancingWallet balancingError
          return (txOutRefs, before ++ (txSkelOut & (txSkelOutValueL % txSkelOutValueContentL) .~ val) : after)
    -- There is no output at the balancing wallet address, or the balancing
    -- policy forces us to create a new output, both yielding the same result.
    _ -> do
      -- We get the optimal candidate, and update the `txSkelOuts` by appending
      -- a new output at the end of the list, to keep the order intact.
      (txOutRefs, val) <- getOptimalCandidate candidatesRaw balancingWallet balancingError
      return (txOutRefs, txSkelOuts ++ [balancingWallet `receives` Value val])
  let newTxSkelIns = txSkelIns <> Map.fromList ((,emptyTxSkelRedeemer) <$> additionalInsTxOutRefs)
  return $ (txSkel & txSkelOutsL .~ newTxSkelOuts) & txSkelInsL .~ newTxSkelIns
