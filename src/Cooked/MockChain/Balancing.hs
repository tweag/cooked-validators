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
  -- Final fee accounts for the size of the transaction and the units consumed
  -- by the execution of scripts from the transaction
  let sizeFees = txFeeFixed + (maxTxSize * txFeePerByte)
      eStepsFees = (eSteps * Rat.numerator priceESteps) `div` Rat.denominator priceESteps
      eMemFees = (eMem * Rat.numerator priceEMem) `div` Rat.denominator priceEMem
  return $ Fee $ sizeFees + eStepsFees + eMemFees

-- | Balances a skeleton and computes optimal fees using a dychotomic search
calcFee :: (MonadBlockChainBalancing m) => Wallet -> Fee -> Fee -> Set Api.TxOutRef -> Wallet -> TxSkel -> m (TxSkel, Fee, Set Api.TxOutRef)
calcFee _ minFee maxFee _ _ _ | minFee >= maxFee = fail "cannot balance"
calcFee balanceWallet minFee@(Fee a) maxFee@(Fee b) collateralIns returnCollateralWallet skel | fee <- Fee $ div (a + b) 2 = do
  attemptedSkel <- computeBalancedTxSkel balanceWallet skel fee
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
    -- picking the first element
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
    Right txBody | Emulator.Coin (Fee -> fee') <- Cardano.evaluateTransactionFee Cardano.ShelleyBasedEraConway pParams txBody nkeys 0 -> return fee'

-- | This creates a balanced skeleton from a given skeleton and fee
-- In other words, this ensures that the following equation holds:
-- input value + minted value = output value + burned value + fee
computeBalancedTxSkel :: (MonadBlockChainBalancing m) => Wallet -> TxSkel -> Fee -> m TxSkel
computeBalancedTxSkel balancingWallet txSkel (Fee (lovelace -> feeValue)) = do
  params <- getParams
  let mintedValue = positivePart $ txSkelMintsValue $ txSkelMints txSkel
      burnedValue = negativePart $ txSkelMintsValue $ txSkelMints txSkel
      outValue = foldOf (txSkelOutsL % folded % txSkelOutValueL) txSkel
  inValue <- txSkelInputValue txSkel
  let left = inValue <> mintedValue
      right = outValue <> burnedValue <> feeValue
      diff = right <> PlutusTx.negate left
      -- what we need to look for in inputs
      missingLeft = positivePart diff
      -- what we need to provide as additional payment
      missingRight = negativePart diff
  balancingUtxosInitial <- runUtxoSearch $ onlyValueOutputsAtSearch balancingWallet `filterWithAlways` outputTxOut
  let alreadyUsedUtxos =
        Map.keys (txSkelIns txSkel)
          <> mapMaybe txSkelReferenceScript (Map.elems $ txSkelIns txSkel)
          <> Set.toList (txSkelInsReference txSkel)
      balancingUtxos = filter ((`notElem` alreadyUsedUtxos) . fst) balancingUtxosInitial
  let candidatesRaw = second (<> missingRight) <$> reachValue balancingUtxos missingLeft (toInteger $ length balancingUtxos)
      candidatesDecorated = second (\val -> (val, Script.fromValue val, getTxSkelOutMinAda params $ paysPK balancingWallet val)) <$> candidatesRaw
      candidatesFiltered = [(lv, (fst <$> l, val)) | (l, (val, Script.Lovelace lv, Right minLv)) <- candidatesDecorated, minLv <= lv]
  case sortBy (compare `on` fst) candidatesFiltered of
    [] -> throwError $ MCEUnbalanceable (MCEUnbalNotEnoughFunds balancingWallet missingLeft) txSkel
    (_, (txOutRefs, val)) : _ ->
      return $
        txSkel
          { txSkelOuts = txSkelOuts txSkel ++ [paysPK balancingWallet val],
            txSkelIns = txSkelIns txSkel <> Map.fromList ((,TxSkelNoRedeemerForPK) <$> txOutRefs)
          }
