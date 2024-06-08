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

  -- We collet the balancing utxos based on the associated options. We filter
  -- out utxos already used in the input skeleton
  (filter ((`notElem` txSkelKnownTxOutRefs skelUnbal) . fst) -> balancingUtxos) <-
    runUtxoSearch $ case txOptBalancingUtxos (txSkelOpts skelUnbal) of
      BalancingUtxosAutomatic -> onlyValueOutputsAtSearch balancingWallet `filterWithAlways` outputTxOut
      BalancingUtxosWith utxos -> txOutByRefSearch (Set.toList utxos) `filterWithPure` isPKOutput `filterWithAlways` outputTxOut

  -- We either return the original skeleton with default fees and associated
  -- collaterals when no balancing is requested. Or, we return the balanced
  -- skeleton with adjusted fees and collaterals, which are computed with a
  -- maximum of 5 balancing iterations (empirically sufficient).
  (txSkelBal, fee, adjustedCollateralIns) <-
    if txOptBalance . txSkelOpts $ skelUnbal
      then calcFee balancingWallet (Fee 0) initialFee collateralIns balancingUtxos returnCollateralWallet skelUnbal
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
  -- Final fee accounts for the size of the transaction and the units consumed
  -- by the execution of scripts from the transaction
  let sizeFees = txFeeFixed + (maxTxSize * txFeePerByte)
      eStepsFees = (eSteps * Rat.numerator priceESteps) `div` Rat.denominator priceESteps
      eMemFees = (eMem * Rat.numerator priceEMem) `div` Rat.denominator priceEMem
  return $ Fee $ sizeFees + eStepsFees + eMemFees

-- | Balances a skeleton and computes optimal fees using a dychotomic search
calcFee :: (MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> Set Api.TxOutRef -> [(Api.TxOutRef, Api.TxOut)] -> Wallet -> TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef)
calcFee _ minFee maxFee _ _ _ _ | minFee >= maxFee = fail "cannot balance"
calcFee balanceWallet minFee@(Fee a) maxFee@(Fee b) collateralIns balancingUtxos returnCollateralWallet skel | fee <- Fee $ div (a + b) 2 = do
  attemptedSkel <- computeBalancedTxSkel balanceWallet balancingUtxos skel fee
  adjustedCollateralIns <- collateralInsFromFees fee collateralIns returnCollateralWallet
  newFee <- estimateTxSkelFee attemptedSkel fee adjustedCollateralIns returnCollateralWallet
  case newFee - fee of
    x | x == 0 || (x < 0 && (b - a) == 1) -> return (attemptedSkel, newFee, adjustedCollateralIns)
    x | x < 0 -> calcFee balanceWallet minFee fee collateralIns balancingUtxos returnCollateralWallet skel
    _ -> calcFee balanceWallet (fee + 1) maxFee collateralIns balancingUtxos returnCollateralWallet skel

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

-- | The main computing function for optimal balancing and collaterals. It
-- computes the subsets of a set of UTxOs that sum up to a certain target. It
-- stops when the target is reached, not adding superfluous UTxOs. Despite
-- optimizations, this function is theoretically in 2^n where n is the number of
-- candidate UTxOs. Use with caution.
reachValue ::
  -- | The candidates UTxOs with their associated output
  [(Api.TxOutRef, Api.TxOut)] ->
  -- | The target value to be reached
  Api.Value ->
  -- | The maximum number of elements in the output subsets
  Integer ->
  -- | Returns subsets of UTxOs with the surplus value
  [([(Api.TxOutRef, Api.TxOut)], Api.Value)]
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
  reachValue t target maxEls
    ++ ( if snd (Api.split target) PlutusTx./\ hVal == mempty
           then []
           else first (h :) <$> reachValue t (target <> PlutusTx.negate hVal) (maxEls - 1)
       )

-- | This function is essentially a copy of
-- https://github.com/input-output-hk/plutus-apps/blob/d4255f05477fd8477ee9673e850ebb9ebb8c9657/plutus-ledger/src/Ledger/Fee.hs#L19
estimateTxSkelFee :: (MonadBlockChainBalancing m) => TxSkel -> Fee -> Set Api.TxOutRef -> Wallet -> m Fee
estimateTxSkelFee skel fee collateralIns returnCollateralWallet = do
  params <- getParams
  managedData <- txSkelInputData skel
  managedTxOuts <- lookupUtxosPl $ txSkelKnownTxOutRefs skel <> Set.toList collateralIns
  managedValidators <- txSkelInputValidators skel
  txBodyContent <- case generateBodyContent fee returnCollateralWallet collateralIns params managedData managedTxOuts managedValidators skel of
    Left err -> throwError $ MCEGenerationError err
    Right txBodyContent -> return txBodyContent
  let nkeys = Cardano.estimateTransactionKeyWitnessCount txBodyContent
      pParams = Emulator.pEmulatorPParams params
  case Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway txBodyContent of
    Left err -> throwError $ MCEGenerationError (TxBodyError "Error creating body when estimating fees" err)
    Right txBody | Emulator.Coin (Fee -> fee') <- Cardano.evaluateTransactionFee Cardano.ShelleyBasedEraConway pParams txBody nkeys 0 -> return fee'

-- | This creates a balanced skeleton from a given skeleton and fee
-- In other words, this ensures that the following equation holds:
-- input value + minted value = output value + burned value + fee
computeBalancedTxSkel :: (MonadBlockChainBalancing m) => Wallet -> [(Api.TxOutRef, Api.TxOut)] -> TxSkel -> Fee -> m TxSkel
computeBalancedTxSkel balancingWallet balancingUtxos txSkel@TxSkel {..} (Fee (lovelace -> feeValue)) = do
  params <- getParams
  let (burnedValue, mintedValue) = Api.split $ txSkelMintsValue txSkelMints
      outValue = foldOf (txSkelOutsL % folded % txSkelOutValueL) txSkel
  inValue <- txSkelInputValue txSkel
  let (missingRight, missingLeft) = Api.split $ outValue <> burnedValue <> feeValue <> PlutusTx.negate (inValue <> mintedValue)
      candidatesRaw = second (<> missingRight) <$> reachValue balancingUtxos missingLeft (toInteger $ length balancingUtxos)
      candidatesDecorated = second (\val -> (val, Script.fromValue val, getTxSkelOutMinAda params $ paysPK balancingWallet val)) <$> candidatesRaw
      candidatesFiltered = [(lv, (fst <$> l, val)) | (l, (val, Script.Lovelace lv, Right minLv)) <- candidatesDecorated, minLv <= lv]
  case sortBy (compare `on` fst) candidatesFiltered of
    [] -> throwError $ MCEUnbalanceable (MCEUnbalNotEnoughFunds balancingWallet missingLeft) txSkel
    (_, (txOutRefs, val)) : _ ->
      let newTxSkelOuts = case break (\(Pays output) -> outputAddress output == walletAddress balancingWallet) txSkelOuts of
            _ | DontAdjustExistingOutput <- txOptBalanceOutputPolicy txSkelOpts -> txSkelOuts ++ [paysPK balancingWallet val]
            (_, []) -> txSkelOuts ++ [paysPK balancingWallet val]
            (l, h : t) -> l ++ (h & txSkelOutValueL .~ ((h ^. txSkelOutValueL) <> val)) : t
          newTxSkelIns = txSkelIns <> Map.fromList ((,TxSkelNoRedeemerForPK) <$> txOutRefs)
       in return $ (txSkel & txSkelOutsL .~ newTxSkelOuts) & txSkelInsL .~ newTxSkelIns
