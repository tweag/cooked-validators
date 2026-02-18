-- | This module handles auto-balancing of transaction skeleton. This includes
-- computation of fees and collaterals because their computation cannot be
-- separated from the balancing.
module Cooked.MockChain.Balancing
  ( ExtendedTxSkel (..),
    balanceTxSkel,
    getMinAndMaxFee,
    estimateTxSkelFee,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad
import Cooked.MockChain.AutoFilling
import Cooked.MockChain.Common
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Body
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.Log
import Cooked.MockChain.Read
import Cooked.MockChain.UtxoSearch
import Cooked.Skeleton
import Data.ByteString qualified as BS
import Data.Function
import Data.List (find, partition)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio qualified as Rat
import Data.Set qualified as Set
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Lens.Micro.Extras qualified as Micro
import Lens.Micro.Extras qualified as MicroLens
import Optics.Core
import Optics.Core.Extras
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Polysemy
import Polysemy.Error
import Polysemy.Fail

type Body = Cardano.TxBody Cardano.ConwayEra

-- | A `TxSkel` with extra pieces of information produced during balancing
data ExtendedTxSkel = ExtendedTxSkel
  { -- | The skeleton itself
    eSkel :: TxSkel,
    -- | The fee associated with this skeleton
    eFee :: Fee,
    -- | The optional collateras associated with this skeleton
    eMCollaterals :: Maybe Collaterals,
    -- | The Cardano body generated from this skeleton
    eMBody :: Body
  }

-- | This is the main entry point of our balancing mechanism. This function
-- takes a skeleton and returns a (possibly) balanced skeleton alongside the
-- associated fee, collateral inputs and return collateral user, which might
-- be empty when no script is involved in the transaction. The options from the
-- skeleton control whether it should be balanced, and how to compute its
-- associated elements.
balanceTxSkel ::
  (Members '[MockChainRead, MockChainLog, Error MockChainError, Error Ledger.ToCardanoError, Fail] effs) =>
  TxSkel ->
  Sem effs ExtendedTxSkel
balanceTxSkel skelUnbal@TxSkel {..} = do
  -- We retrieve the possible balancing user. Any extra payment will be
  -- redirected to them, and utxos will be taken from their wallet if associated
  -- with the @BalancingUtxosFromBalancingUser@ policy
  balancingUser <- case txSkelOptBalancingPolicy txSkelOpts of
    BalanceWithFirstSignatory -> case txSkelSignatories of
      [] -> throw $ MCEBalancingError MissingBalancingUser
      bw : _ -> return $ Just $ UserPubKey bw
    BalanceWith bUser -> return $ Just $ UserPubKey bUser
    DoNotBalance -> return Nothing

  -- We retrieve the number of scripts involved in the transaction. This is used
  -- to compute the maximum possible fee, as each of those script with
  -- contribute, through its execution units, to the cost of the transaction.
  nbOfScripts <- fromIntegral . length <$> txSkelAllScripts skelUnbal

  -- The protocol parameters indirectly dictate a minimal and maximal value for a
  -- single transaction fee, which we retrieve.
  (minFee, maxFee) <- getMinAndMaxFee nbOfScripts

  -- We collect potential collateral inputs candidates, and return collateral
  -- user. They will be absent when the transaction does not involve script and
  -- thus does not require collaterals.
  mCollaterals <- do
    case (nbOfScripts == 0, txSkelOptCollateralUtxos txSkelOpts) of
      -- No script involved, but manual collateral UTxOs provided
      (True, CollateralUtxosFromSet utxos _) -> logEvent (MCLogUnusedCollaterals $ Right utxos) >> return Nothing
      -- No script involved, but manual collateral user provided
      (True, CollateralUtxosFromUser cUser) -> logEvent (MCLogUnusedCollaterals $ Left $ UserPubKey cUser) >> return Nothing
      -- No script involved, and no particular collateral option provided
      (True, CollateralUtxosFromBalancingUser) -> return Nothing
      -- Some scripts involved, and a specific set of UTxOs, alongside a
      -- collateral user provided. In this case, we just return them.
      (False, CollateralUtxosFromSet utxos rUser) -> return $ Just (utxos, UserPubKey rUser)
      -- Some scripts involved, and a specific collateral user provided.
      -- We fetch vanilla UTxOs from this user and return them.
      (False, CollateralUtxosFromUser (Script.toPubKeyHash -> cUser)) ->
        Just . (,UserPubKey cUser) . Set.fromList
          <$> getTxOutRefs (utxosAtSearch cUser ensureOnlyValueOutputs)
      -- Some scripts involved, and no specific collateral options provided.
      (False, CollateralUtxosFromBalancingUser) -> case balancingUser of
        -- If no balancing wallet exists, we throw an error
        Nothing -> throw $ MCEBalancingError MissingBalancingUser
        -- If a balancing wallet exists, we use it as collateral user
        Just bUser -> Just . (,bUser) . Set.fromList <$> getTxOutRefs (utxosAtSearch bUser ensureOnlyValueOutputs)

  -- At this point, the presence (or absence) of balancing user dictates
  -- whether the transaction should be automatically balanced or not.
  case balancingUser of
    Nothing -> do
      -- The balancing should not be performed. We still adjust the collaterals
      -- though around a provided fee, or the maximum fee.
      let fee = case txSkelOptFeePolicy txSkelOpts of
            AutoFeeComputation -> maxFee
            ManualFee fee' -> fee'
      mCols <- collateralsFromFee fee mCollaterals
      cBody <- txSkelToTxBody skelUnbal fee mCols
      return $ ExtendedTxSkel skelUnbal fee mCols cBody
    Just bUser -> do
      -- The balancing should be performed. We collect the candidates balancing
      -- utxos based on the associated policy
      balancingUtxos <-
        case txSkelOptBalancingUtxos txSkelOpts of
          BalancingUtxosFromBalancingUser -> getTxOutRefsAndOutputs $ utxosAtSearch bUser ensureOnlyValueOutputs
          BalancingUtxosFromSet utxos ->
            -- We resolve the given set of utxos
            getTxOutRefsAndOutputs (txSkelOutByRefSearch' (Set.toList utxos))
              -- We filter out those belonging to scripts, while throwing a
              -- warning if any was actually discarded.
              >>= filterAndWarn (is (txSkelOutOwnerL % userPubKeyHashAT) . snd) "They belong to scripts."
          -- We filter the candidate utxos by removing those already present in the
          -- skeleton, throwing a warning if any was actually discarded
          >>= filterAndWarn ((`notElem` txSkelKnownTxOutRefs skelUnbal) . fst) "They are already used in the skeleton."

      case txSkelOptFeePolicy txSkelOpts of
        -- If fees are left for us to compute, we run a dichotomic search. This
        -- is full auto mode, the most powerful but time-consuming.
        AutoFeeComputation ->
          computeFeeAndBalance bUser minFee maxFee balancingUtxos mCollaterals skelUnbal
        -- If fee are provided manually, we adjust the collaterals and the
        -- skeleton around them directly.
        ManualFee fee -> do
          mCols <- collateralsFromFee fee mCollaterals
          balancedSkel <- computeBalancedTxSkel bUser balancingUtxos skelUnbal fee
          cBody <- txSkelToTxBody balancedSkel fee mCols
          return $ ExtendedTxSkel balancedSkel fee mCols cBody
  where
    filterAndWarn f s l
      | (ok, toInteger . length -> koLength) <- partition f l =
          unless (koLength == 0) (logEvent $ MCLogDiscardedUtxos koLength s) >> return ok

-- | Computes optimal fee for a given skeleton and balances it around those fees.
-- This uses a dichotomic search for an optimal "balanceable around" fee.
computeFeeAndBalance ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError, Fail] effs) =>
  Peer ->
  Fee ->
  Fee ->
  Utxos ->
  Maybe (CollateralIns, Peer) ->
  TxSkel ->
  Sem effs ExtendedTxSkel
computeFeeAndBalance _ minFee maxFee _ _ _
  | minFee > maxFee =
      fail "Unreachable case, please report a bug at https://github.com/tweag/cooked-validators/issues"
computeFeeAndBalance balancingUser minFee maxFee balancingUtxos mCollaterals skel = do
  let fee = (minFee + maxFee) `div` 2
  -- The fee interval is non-empty. We attempt to balance around its central
  -- point, and handle possible failures.
  attemptedBalancing <- catch
    (Just <$> attemptBalancingAndCollaterals balancingUser balancingUtxos fee mCollaterals skel)
    $ \case
      -- If it fails, and the remaining fee interval is not reduced to the
      -- current fee attempt, we return `Nothing` which signifies that we
      -- need to keep searching. Otherwise, the whole balancing process
      -- fails and we spread the error.
      MCEBalancingError {} | fee > minFee -> return Nothing
      err -> throw err

  case attemptedBalancing of
    -- The skeleton was not balanceable, we try strictly smaller fee
    Nothing -> computeFeeAndBalance balancingUser minFee (fee - 1) balancingUtxos mCollaterals skel
    -- The skeleton was balanceable, we cannot try smaller fee, and
    -- the used fee is sufficient for the generated body. All good!
    Just extendedTxSkel | minFee == maxFee, eFee extendedTxSkel <= fee -> return extendedTxSkel
    -- The skeleton was balanceable, we cannot try smaller fee, but
    -- the used fee is insufficient for the generated body
    Just _ | minFee == maxFee -> throw $ MCEBalancingError $ NotEnoughFundForProperFee balancingUser
    -- Current fee is insufficient, we look on the right (strictly)
    Just extendedTxSkel
      | eFee extendedTxSkel > fee ->
          computeFeeAndBalance balancingUser (fee + 1) maxFee balancingUtxos mCollaterals skel
    -- Current fee is sufficient, but the set of balancing utxos cannot
    -- necessarily account for less fee, since it was (magically) exactly enough
    -- to compensate for the missing value. Reducing the fee would ruin this
    -- perfect balancing and force an output to be created at the balancing user
    -- address, thus we cannot assume the actual estimated fee can be accounted
    -- for with the current set of balancing utxos and cannot speed up search.
    Just extendedSkel
      | txSkelValueInOutputs (eSkel extendedSkel) == txSkelValueInOutputs skel ->
          computeFeeAndBalance balancingUser minFee fee balancingUtxos mCollaterals skel
    -- Current fee is sufficient, and the set of utxo could account for
    -- less fee by feeding into whatever output already goes back to the
    -- balancing user. We can speed up search, because the current
    -- attempted skeleton could necessarily account for the estimated
    -- fee of the input skeleton.
    Just extendedSkel ->
      computeFeeAndBalance balancingUser minFee (eFee extendedSkel) balancingUtxos mCollaterals skel

-- | Helper function to group the three real steps of the balancing: balance a
-- skeleton around a given fee, compute the associated collateral inputs, and
-- compute the new fee from those elements. It the process, also returns the
-- generated body for the new skeleton.
attemptBalancingAndCollaterals ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError, Fail] effs) =>
  Peer ->
  Utxos ->
  Fee ->
  Maybe (CollateralIns, Peer) ->
  TxSkel ->
  Sem effs ExtendedTxSkel
attemptBalancingAndCollaterals balancingUser balancingUtxos fee mCollaterals skel = do
  newSkel <- computeBalancedTxSkel balancingUser balancingUtxos skel fee
  mCols <- collateralsFromFee fee mCollaterals
  (body, newFee) <- estimateTxSkelFee newSkel fee mCols
  return $ ExtendedTxSkel newSkel newFee mCols body

-- | This selects a subset of suitable collateral inputs from a given set while
-- accounting for the ratio to respect between fees and total collaterals, the
-- min ada requirements in the associated return collateral and the maximum
-- number of collateral inputs authorized by protocol parameters.
collateralsFromFee ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  -- | The fee from which these collaterals should be computed
  Fee ->
  -- | The optional candidate UTxOs to be used as collaterals, alongside the
  -- peer who should receive the return collateral output
  Maybe (CollateralIns, Peer) ->
  -- | Returns the collaterals computed from the above. Raises an error if no
  -- such collateral can be found.
  Sem effs (Maybe Collaterals)
collateralsFromFee _ Nothing = return Nothing
collateralsFromFee fee (Just (collateralIns, returnCollateralUser)) = do
  -- We retrieve the protocal parameters
  params <- Emulator.pEmulatorPParams <$> getParams
  -- We retrieve the max number of collateral inputs, with a default of 10. In
  -- practice this will be around 3.
  let nbMax = toInteger $ MicroLens.view Conway.ppMaxCollateralInputsL params
  -- We retrieve the percentage to respect between fees and total collaterals
  let percentage = toInteger $ MicroLens.view Conway.ppCollateralPercentageL params
  -- We compute the total collateral to be associated to the transaction as a
  -- value. This will be the target value to be reached by collateral inputs. We
  -- add one because of ledger requirement which seem to round up this value.
  let totalCollateral = Script.lovelace . (+ 1) . (`div` 100) . (* percentage) $ fee
  -- Collateral tx outputs sorted by decreasing ada amount
  collateralTxOuts <- getTxOutRefsAndOutputs $ txSkelOutByRefSearch' $ Set.toList collateralIns
  -- Candidate subsets of utxos to be used as collaterals
  reachedValue <- reachValue collateralTxOuts totalCollateral nbMax $ Right returnCollateralUser
  -- A value might, or might not have been reached
  case reachedValue of
    -- If no value was reached, the input UTxOs are insufficient to provide
    -- the necessary collaterals, and thus an error is raised
    Nothing -> throw $ MCEBalancingError $ NoSuitableCollateral fee percentage totalCollateral
    -- If a value was reached, we return it alongside the return collaterals
    Just (oRefs, returnOutput) -> return $ Just (Set.fromList oRefs, returnOutput)

reachValue ::
  forall effs.
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  -- | The Utxos available to reach the value
  Utxos ->
  -- | The target value to reach
  Api.Value ->
  -- | The maximum number of Utxos allowed to reach the target. This is used
  -- when there is a hard limit (for collaterals for instance) or when the
  -- amount of input UTxOs is huge and thus the search needs to be limited.
  Integer ->
  -- | Either the output to which the generated surplus needs to be attached, or
  -- the users to which this surplus should be sent
  Either TxSkelOut Peer ->
  -- | Returns a possible solution. The solution is a list of new inputs, and
  -- the surplus output, which is either built from scratch or from the provided
  -- surplus output, if any.
  Sem effs (Maybe ([Api.TxOutRef], Maybe TxSkelOut))
reachValue utxos target fuel outputOrUser = do
  -- We retrieve the current protocol version, which is going to be used to
  -- compute the size of the inputs and outputs added by this function
  Cardano.ProtVer majorVersion _ <- Micro.view Conway.ppProtocolVersionL . Emulator.emulatorPParams <$> getParams
  -- We annotate @outputOrUser@ with the size of the existing output, if any
  outputOrUser' <- case outputOrUser of
    Left output -> Left . (output,) <$> outputSize majorVersion output
    Right user -> return $ Right user
  -- We annotate each of the provided inputs with their sizes
  utxos' <- forM utxos $ \(oRef, output) -> (oRef,,view txSkelOutValueL output) <$> inputSize majorVersion oRef
  -- We call the main computing function, @go@, by feeding it an initial
  -- available amount, computed from the available inputs. This will avoid any
  -- unnecessary recomputation of this value.
  fmap (\(oRefs, mOut, _) -> (oRefs, mOut))
    <$> go majorVersion utxos' target fuel outputOrUser' (mconcat ((\(_, _, val) -> val) <$> utxos'))
  where
    go ::
      -- Current protocol major version
      Cardano.Version ->
      -- Currently available UTxOs, decreasing in recursive calls
      [(Api.TxOutRef, Integer, Api.Value)] ->
      -- Target value
      Api.Value ->
      -- Fuel (max number of UTxOs to pick)
      Integer ->
      -- Existing output where the surplus needs to be appended, or the peer
      -- to whom this surplus should be paid
      Either (TxSkelOut, Integer) Peer ->
      -- Total value of the available inputs
      Api.Value ->
      -- Returns a solution when one exists. This solution contains the inputs
      -- to add, the possible surplus payment and the total size added by these
      -- elements to the transaction.
      Sem effs (Maybe ([Api.TxOutRef], Maybe TxSkelOut, Integer))
    -- The target is reached. There might be surplus which we have to handle.
    go majorVersion goUtxos goTarget goFuel goOutputOrUser goAvailable | goTarget `Api.leq` mempty = do
      let remainder = PlutusTx.negate goTarget
          newOutput = either (over txSkelOutValueL (<> remainder) . fst) (`receives` Value remainder) goOutputOrUser
          newOutputValue = view txSkelOutValueL newOutput
      minAda <- Api.Lovelace <$> getTxSkelOutMinAda newOutput
      case newOutputValue of
        -- There is no surplus
        newVal | newVal == mempty -> return $ Just ([], Nothing, 0)
        -- There is a surplus and it contains enough ADA
        newVal | view valueLovelaceL newVal >= minAda -> do
          -- We compute the cost of the new output
          newCost <- outputSize majorVersion newOutput
          -- And compare it with the cost of the existing output
          return $ Just ([], Just newOutput, either ((newCost -) . snd) (const newCost) goOutputOrUser)
        -- There is a surplus which does not contain enough ADA
        (Script.toValue . (minAda -) . view valueLovelaceL -> missingAdaValue) -> do
          -- We need to run a new search with a target increased by the missing
          -- amount of ADA. For that purpose, we also need to increase the
          -- surplus payment with the same amout, to keep everything balanced.
          -- As a consequence, we also need to add bytes to the transaction.
          (sizeAdded, goOutputOrUser') <- case goOutputOrUser of
            -- If the surplus already exist, we add @missingAdaValue@ to it
            Left (output, existingSize) -> do
              let enlargedOutput = over txSkelOutValueL (<> missingAdaValue) output
              newSize <- outputSize majorVersion enlargedOutput
              return (newSize - existingSize, Left (enlargedOutput, newSize))
            -- If it does not, we create it and couple it with its size
            Right user -> do
              let output = user `receives` Value missingAdaValue
              size <- outputSize majorVersion output
              return (size, Left (output, size))
          -- We keep looking with a greater target, surplus and size
          over (_Just % _3) (+ sizeAdded)
            <$> go majorVersion goUtxos (goTarget <> missingAdaValue) goFuel goOutputOrUser' goAvailable
    -- We have not reached a solution, but we don't have fuel anymore
    go _ _ _ goFuel _ _ | goFuel <= 0 = return Nothing
    -- We have not reached a soultion, but no more UTxOs are available
    go _ [] _ _ _ _ = return Nothing
    -- We have not reached a solution, but the total available value is
    -- insufficient to ever find one
    go _ _ goTarget _ _ goAvailable | not $ goTarget `Api.leq` goAvailable = return Nothing
    -- We have not yet found a solution, but there are still available UTxOs
    go majorVersion ((hOref, hSize, hValue) : t) goTarget goFuel goOutputOrUser ((<> PlutusTx.negate hValue) -> goAvailable) = do
      -- We try to find a solution by dropping the head
      dropH <- go majorVersion t goTarget goFuel goOutputOrUser goAvailable
      -- We also try to find a solution by picking the head
      pickH <-
        -- We try to see if the head contributes to reaching the value, i.e. if
        -- it contains assets that help building towards the target.
        if snd (Api.split goTarget) PlutusTx./\ hValue == mempty
          -- If not, we don't bother trying to pick the head
          then return Nothing
          -- If it does, we actually try to pick the head. This means decreasing
          -- most of the recursive parameters of @go@ accordingly
          else do
            pickH' <- go majorVersion t (goTarget <> PlutusTx.negate hValue) (goFuel - 1) goOutputOrUser goAvailable
            return $ (\(oRefs, mOut, size) -> (hOref : oRefs, mOut, hSize + size)) <$> pickH'
      -- We find the optimal solution by comparing both solutions
      return $ case (dropH, pickH) of
        -- Only picking the head yielded a solution, we return it
        (Nothing, _) -> pickH
        -- Only dropping the head yielded a solution, we return it
        (_, Nothing) -> dropH
        -- Both pickding and dropping the head yielded a solution, so we keep
        -- the one that produces the least increase in the transaction size
        (Just (_, _, sizeDrop), Just (_, _, sizePick)) -> if sizeDrop <= sizePick then dropH else pickH

    -- This computes the size of anything that can be serialized
    computeSize majorVersion = fromIntegral . BS.length . Cardano.serialize' majorVersion

    -- This computes the size of a `TxSkelOut`
    outputSize :: Cardano.Version -> TxSkelOut -> Sem effs Integer
    outputSize majorVersion = fmap (computeSize majorVersion . Cardano.toShelleyTxOutAny Cardano.ShelleyBasedEraConway) . toCardanoTxOut

    -- This computes the size of an `Api.TxOutRef` which is almost always
    -- gonna be the same, but can theoretically vary if coming from a
    -- transaction with many outputs.
    inputSize :: Cardano.Version -> Api.TxOutRef -> Sem effs Integer
    inputSize majorVersion = fmap (computeSize majorVersion . Cardano.toShelleyTxIn) . fromEither . Ledger.toCardanoTxIn

-- | Estimates the required fee for a given skeleton with a given initial fee
-- and collaterals
estimateTxSkelFee ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError, Fail] effs) =>
  TxSkel ->
  Fee ->
  Maybe Collaterals ->
  Sem effs (Body, Fee)
estimateTxSkelFee skel fee mCollaterals = do
  -- We retrieve the necessary data to generate the transaction body
  params <- Emulator.pEmulatorPParams <$> getParams
  -- We build the index known to the skeleton
  index <- txSkelToIndex skel mCollaterals
  -- We build the transaction body
  txBody <- txSkelToTxBody skel fee mCollaterals
  -- We retrieve the amount of signatories
  let nbOfSignatories = fromIntegral $ length $ txSkelSignatories skel
  -- We compute the estimated fee
  let Cardano.Coin newFee = Cardano.calculateMinTxFee Cardano.ShelleyBasedEraConway params index txBody nbOfSignatories
  -- We return both the new fee and generated body
  return (txBody, newFee)

-- | This creates a balanced skeleton from a given skeleton and fee. In other
-- words, this ensures that the following equation holds: input value + minted
-- value + withdrawn value = output value + burned value + fee + deposits
computeBalancedTxSkel ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  Peer ->
  Utxos ->
  TxSkel ->
  Fee ->
  Sem effs TxSkel
computeBalancedTxSkel balancingUser balancingUtxos txSkel@TxSkel {..} (Script.lovelace -> feeValue) = do
  -- We compute the necessary values from the skeleton that are part of the
  -- equation, except for the `feeValue` which we already have.
  let (burnedValue, mintedValue) = Api.split $ Script.toValue txSkelMints
      outValue = txSkelValueInOutputs txSkel
      withdrawnValue = txSkelWithdrawnValue txSkel
  inValue <- txSkelInputValue txSkel
  certificatesDepositedValue <- Script.toValue <$> txSkelDepositedValueInCertificates txSkel
  proposalsDepositedValue <- Script.toValue <$> txSkelDepositedValueInProposals txSkel
  -- We compute the values missing in the left and right side of the equation
  let (missingRight', missingLeft') =
        Api.split $
          mconcat
            [ outValue,
              burnedValue,
              feeValue,
              proposalsDepositedValue,
              certificatesDepositedValue,
              PlutusTx.negate inValue,
              PlutusTx.negate mintedValue,
              PlutusTx.negate withdrawnValue
            ]
  -- We need to account for the possible corner case were the inputs are empty
  -- and there is nothing missing on the left, as the ledger does not allow for
  -- transaction to have no inputs. When this is the case, we artificially add a
  -- requirement of 1 lovelace to force the consumption of a dummy input.
  let noInputs = inValue == mempty && missingLeft' == mempty
      missingLeft = if noInputs then Script.lovelace 1 else missingLeft'
      missingRight = if noInputs then missingRight' <> Script.lovelace 1 else missingRight'
  -- We compute the possible existing output that will need to be extended by
  -- the extra surplus created by the balancing. This output is created from
  -- both the extra value on the right, and a possible existing output at the
  -- balancing wallet address when required with @AdjustExistingOutput@.
  let surplusOutputOrUser = case txSkelOptBalanceOutputPolicy txSkelOpts of
        AdjustExistingOutput
          | Just txSkelOut <-
              find ((== Script.toCredential balancingUser) . view txSkelOutCredentialG) txSkelOuts ->
              Left (over txSkelOutValueL (<> missingRight) txSkelOut)
        _ | missingRight == mempty -> Right balancingUser
        _ -> Left (balancingUser `receives` Value missingRight)
  -- We call the main actual balancing algorithm to fetch missing piece, and
  -- retrieve the possible solution, which might not exist.
  let maxNbOfBalancingUtxos = fromMaybe (toInteger $ length balancingUtxos) (txSkelOptMaxNbOfBalancingUtxos txSkelOpts)
  solution <- reachValue balancingUtxos missingLeft maxNbOfBalancingUtxos surplusOutputOrUser
  -- Based on the solution, we compute extra inputs and the new output
  (additionalInsTxOutRefs, newTxSkelOuts) <- case solution of
    -- There is no solution with the provided parameters
    Nothing -> do
      let totalValue = mconcat $ view txSkelOutValueL . snd <$> balancingUtxos
          difference = snd $ Api.split $ missingLeft <> PlutusTx.negate totalValue
      throw $
        MCEBalancingError $
          if difference == mempty
            then NotEnoughFundForExtraMinAda balancingUser
            else NotEnoughFund balancingUser difference
    -- There exists a perfect solution, this is the rarest and easiest
    -- scenario, as the outputs will not change due to balancing. This means
    -- that there was no missing value on the right and the balancing utxos
    -- exactly account for what was missing on the left.
    Just (newORefs, Nothing) -> return (newORefs, txSkelOuts)
    -- There in an existing output at the owner's address and the balancing
    -- policy allows us to adjust it with additional value.
    Just (newORefs, Just newTxSkelOut)
      | AdjustExistingOutput <- txSkelOptBalanceOutputPolicy txSkelOpts,
        (before, _ : after) <- break ((== Script.toCredential balancingUser) . view txSkelOutCredentialG) txSkelOuts ->
          return (newORefs, before ++ (newTxSkelOut : after))
    -- There is no output at the balancing user address, or the balancing
    -- policy forces us to create a new output, both yielding the same result.
    Just (newORefs, Just newTxSkelOut) -> return (newORefs, txSkelOuts ++ [newTxSkelOut])
  let newTxSkelIns = txSkelIns <> Map.fromList ((,emptyTxSkelRedeemer) <$> additionalInsTxOutRefs)
  return $ (txSkel & txSkelOutsL .~ newTxSkelOuts) & txSkelInsL .~ newTxSkelIns

-- | This computes the minimum and maximum possible fee a transaction can cost
-- based on the current protocol parameters and its number of scripts.
-- In the Dijsktra era, this will be modified with new protocol parameters.
-- See https://github.com/IntersectMBO/cardano-ledger/blob/master/docs/adr/2024-08-14_009-refscripts-fee-change.md
-- for more information
getMinAndMaxFee ::
  (Members '[MockChainRead] effs) =>
  Integer ->
  Sem effs (Fee, Fee)
getMinAndMaxFee nbOfScripts = do
  -- We retrieve the necessary parameters to compute the maximum possible fee
  -- for a transaction. There are quite a few of them.
  params <- Emulator.pEmulatorPParams <$> getParams
  let maxTxSize = toInteger $ MicroLens.view Conway.ppMaxTxSizeL params
      Cardano.Coin txFeePerByte = MicroLens.view Conway.ppMinFeeAL params
      Cardano.Coin txFeeFixed = MicroLens.view Conway.ppMinFeeBL params
      Cardano.Prices (Cardano.unboundRational -> priceESteps) (Cardano.unboundRational -> priceEMem) = MicroLens.view Conway.ppPricesL params
      Cardano.ExUnits (toInteger -> eSteps) (toInteger -> eMem) = MicroLens.view Conway.ppMaxTxExUnitsL params
      (Cardano.unboundRational -> refScriptFeePerByte) = MicroLens.view Conway.ppMinFeeRefScriptCostPerByteL params
  -- We compute the components of the maximum possible fee, starting with the
  -- maximum fee associated with the transaction size
  let txSizeMaxFee = maxTxSize * txFeePerByte
  -- maximum fee associated with the number of execution steps for scripts
  let eStepsMaxFee = (eSteps * Rat.numerator priceESteps) `div` Rat.denominator priceESteps
  -- maximum fee associated with the number of execution memory for scripts
  let eMemMaxFee = (eMem * Rat.numerator priceEMem) `div` Rat.denominator priceEMem
  -- maximum fee associated with the size of all reference scripts
  let refScriptsMaxFee = (maxTxSize * Rat.numerator refScriptFeePerByte) `div` Rat.denominator refScriptFeePerByte
  return
    ( -- Minimal fee is just the fixed portion of the fee
      txFeeFixed,
      -- Maximal fee is the fixed portion plus all the other maximum fees
      txFeeFixed + txSizeMaxFee + nbOfScripts * (eStepsMaxFee + eMemMaxFee) + refScriptsMaxFee
    )
