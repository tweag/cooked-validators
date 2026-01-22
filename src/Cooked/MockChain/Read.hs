{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to query the current state of the
-- blockchain.
module Cooked.MockChain.Read
  ( -- * The `MockChainRead` effect
    MockChainRead,
    runMockChainRead,

    -- * Queries related to protocol parameters
    getParams,
    govActionDeposit,
    dRepDeposit,
    stakeAddressDeposit,
    stakePoolDeposit,

    -- * Queries related to `Cooked.Skeleton.TxSkel`
    txSkelDepositedValueInCertificates,
    txSkelDepositedValueInProposals,
    txSkelAllScripts,
    txSkelInputScripts,
    txSkelInputValue,

    -- * Queries related to timing
    currentSlot,
    currentMSRange,
    getEnclosingSlot,
    slotRangeBefore,
    slotRangeAfter,
    slotToMSRange,

    -- * Queries related to fetching UTxOs
    allUtxos,
    utxosAt,
    txSkelOutByRef,
    utxosFromCardanoTx,
    lookupUtxos,
    previewByRef,
    viewByRef,

    -- * Other queries
    getConstitutionScript,
    getCurrentReward,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.MockChain.Common
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Credential (toStakeCredential)
import Cooked.MockChain.MockChainState
import Cooked.Skeleton
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State

-- | An effect that offers primitives to query the current state of the
-- mockchain. As its name suggests, this effect is read-only and does not alter
-- the state in any way.
data MockChainRead :: Effect where
  GetParams :: MockChainRead m Emulator.Params
  TxSkelOutByRef :: Api.TxOutRef -> MockChainRead m TxSkelOut
  CurrentSlot :: MockChainRead m Ledger.Slot
  AllUtxos :: MockChainRead m Utxos
  UtxosAt :: (Script.ToAddress a) => a -> MockChainRead m Utxos
  GetConstitutionScript :: MockChainRead m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainRead m (Maybe Api.Lovelace)

makeSem_ ''MockChainRead

-- | The interpretation for read-only effect in the blockchain state
runMockChainRead ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error Ledger.ToCardanoError,
         Error MockChainError
       ]
      effs
  ) =>
  Sem (MockChainRead : effs) a ->
  Sem effs a
runMockChainRead = interpret $ \case
  GetParams -> gets mcstParams
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . mcstOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ MCEUnknownOutRef oRef
  AllUtxos -> fetchUtxos $ const True
  UtxosAt (Script.toAddress -> addr) -> fetchUtxos $ (== addr) . Script.toAddress
  CurrentSlot -> gets $ view $ mcstLedgerStateL % to Emulator.getSlot
  GetConstitutionScript -> gets $ view mcstConstitutionL
  GetCurrentReward (Script.toCredential -> cred) -> do
    stakeCredential <- toStakeCredential cred
    gets $
      preview $
        mcstLedgerStateL
          % to (Emulator.getReward stakeCredential)
          % _Just
          % to coerce
  where
    fetchUtxos decide =
      gets $
        toListOf $
          mcstOutputsL
            % to Map.toList
            % traversed
            % filtered (snd . snd)
            % filtered (decide . fst . snd)
            % to (fmap fst)

-- | Returns the emulator parameters, including protocol parameters
getParams ::
  (Member MockChainRead effs) =>
  Sem effs Emulator.Params

-- | Retrieves the required governance action deposit amount
govActionDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
govActionDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppGovActionDepositL
    . Emulator.emulatorPParams

-- | Retrieves the required drep deposit amount
dRepDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
dRepDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppDRepDepositL
    . Emulator.emulatorPParams

-- | Retrieves the required stake address deposit amount
stakeAddressDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
stakeAddressDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppKeyDepositL
    . Emulator.emulatorPParams

-- | Retrieves the required stake pool deposit amount
stakePoolDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
stakePoolDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppPoolDepositL
    . Emulator.emulatorPParams

-- | Retrieves the total amount of lovelace deposited in certificates in this
-- skeleton. Note that unregistering a staking address or a dRep lead to a
-- negative deposit (a withdrawal, in fact) which means this function can return
-- a negative amount of lovelace, which is intended. The deposited amounts are
-- dictated by the current protocol parameters, and computed as such.
txSkelDepositedValueInCertificates ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs Api.Lovelace
txSkelDepositedValueInCertificates txSkel = do
  sDep <- stakeAddressDeposit
  dDep <- dRepDeposit
  pDep <- stakePoolDeposit
  return $
    foldOf
      ( txSkelCertificatesL
          % traversed
          % to
            ( \case
                TxSkelCertificate _ StakingRegister {} -> sDep
                TxSkelCertificate _ StakingRegisterDelegate {} -> sDep
                TxSkelCertificate _ StakingUnRegister {} -> -sDep
                TxSkelCertificate _ DRepRegister {} -> dDep
                TxSkelCertificate _ DRepUnRegister {} -> -dDep
                TxSkelCertificate _ PoolRegister {} -> pDep
                -- There is no special case for 'PoolRetire' because the deposit
                -- is given back to the reward account.
                _ -> Api.Lovelace 0
            )
      )
      txSkel

-- | Retrieves the total amount of lovelace deposited in proposals in this
-- skeleton (equal to `govActionDeposit` times the number of proposals)
txSkelDepositedValueInProposals ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs Api.Lovelace
txSkelDepositedValueInProposals TxSkel {txSkelProposals} =
  govActionDeposit
    <&> Api.Lovelace
    . (toInteger (length txSkelProposals) *)
    . Api.getLovelace

-- | Returns all scripts involved in this 'TxSkel'
txSkelAllScripts ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelAllScripts txSkel = do
  txSkelSpendingScripts <- txSkelInputScripts txSkel
  return
    ( txSkelMintingScripts txSkel
        <> txSkelWithdrawingScripts txSkel
        <> txSkelProposingScripts txSkel
        <> txSkelCertifyingScripts txSkel
        <> txSkelSpendingScripts
    )

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelInputScripts =
  fmap catMaybes
    . mapM (previewByRef (txSkelOutOwnerL % userVScriptAT))
    . Map.keys
    . txSkelIns

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs Api.Value
txSkelInputValue =
  fmap mconcat
    . mapM (viewByRef txSkelOutValueL)
    . Map.keys
    . txSkelIns

-- | Returns the current slot
currentSlot ::
  (Member MockChainRead effs) =>
  Sem effs Ledger.Slot

-- | Returns the closed ms interval corresponding to the current slot
currentMSRange ::
  (Members '[MockChainRead, Fail] effs) =>
  Sem effs (Api.POSIXTime, Api.POSIXTime)
currentMSRange = slotToMSRange =<< currentSlot

-- | Return the slot that contains the given time. See 'slotToMSRange' for
-- some satisfied equational properties.
getEnclosingSlot ::
  (Member MockChainRead effs) =>
  Api.POSIXTime ->
  Sem effs Ledger.Slot
getEnclosingSlot t =
  getParams
    <&> (`Emulator.posixTimeToEnclosingSlot` t)
    . Emulator.pSlotConfig

-- | The infinite range of slots ending before or at the given time
slotRangeBefore ::
  (Members '[MockChainRead, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs Ledger.SlotRange
slotRangeBefore t = do
  n <- getEnclosingSlot t
  (_, b) <- slotToMSRange n
  -- If the given time @t@ happens to be the last ms of its slot, we can include
  -- the whole slot. Otherwise, the only way to be sure that the returned slot
  -- range contains no time after @t@ is to go to the preceding slot.
  return $ Api.to $ if t == b then n else n - 1

-- | The infinite range of slots starting after or at the given time
slotRangeAfter ::
  (Members '[MockChainRead, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToMSRange n
  return $ Api.from $ if t == a then n else n + 1

-- | Returns the closed ms interval corresponding to the slot with the given
-- number. It holds that
--
-- > slotToMSRange (getEnclosingSlot t) == (a, b)    ==>   a <= t <= b
--
-- and
--
-- > slotToMSRange n == (a, b)   ==>   getEnclosingSlot a == n && getEnclosingSlot b == n
--
-- and
--
-- > slotToMSRange n == (a, b)   ==>   getEnclosingSlot (a-1) == n-1 && getEnclosingSlot (b+1) == n+1
slotToMSRange ::
  ( Members '[MockChainRead, Fail] effs,
    Integral i
  ) =>
  i ->
  Sem effs (Api.POSIXTime, Api.POSIXTime)
slotToMSRange (fromIntegral -> slot) = do
  slotConfig <- Emulator.pSlotConfig <$> getParams
  case Emulator.slotToPOSIXTimeRange slotConfig slot of
    Api.Interval
      (Api.LowerBound (Api.Finite l) leftclosed)
      (Api.UpperBound (Api.Finite r) rightclosed) ->
        return
          ( if leftclosed then l else l + 1,
            if rightclosed then r else r - 1
          )
    _ -> fail "Unexpected unbounded slot: please report a bug at https://github.com/tweag/cooked-validators/issues"

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member MockChainRead effs) =>
  Sem effs Utxos

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member MockChainRead effs,
    Script.ToAddress a
  ) =>
  a ->
  Sem effs Utxos

-- | Returns an output given a reference to it
txSkelOutByRef ::
  (Member MockChainRead effs) =>
  Api.TxOutRef ->
  Sem effs TxSkelOut

-- | Retrieves the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx ::
  (Member MockChainRead effs) =>
  Ledger.CardanoTx ->
  Sem effs [(Api.TxOutRef, TxSkelOut)]
utxosFromCardanoTx =
  mapM (\txOutRef -> (txOutRef,) <$> txSkelOutByRef txOutRef)
    . fmap (Ledger.fromCardanoTxIn . snd)
    . Ledger.getCardanoTxOutRefs

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
lookupUtxos ::
  (Member MockChainRead effs) =>
  [Api.TxOutRef] ->
  Sem effs (Map Api.TxOutRef TxSkelOut)
lookupUtxos =
  foldM
    (\m oRef -> flip (Map.insert oRef) m <$> txSkelOutByRef oRef)
    Map.empty

-- | Retrieves an output and views a specific element out of it
viewByRef ::
  ( Member MockChainRead effs,
    Is g A_Getter
  ) =>
  Optic' g is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs c
viewByRef optic = (view optic <$>) . txSkelOutByRef

-- | Retrieves an output and previews a specific element out of it
previewByRef ::
  ( Member MockChainRead effs,
    Is af An_AffineFold
  ) =>
  Optic' af is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs (Maybe c)
previewByRef optic = (preview optic <$>) . txSkelOutByRef

-- | Gets the current official constitution script
getConstitutionScript ::
  (Member MockChainRead effs) =>
  Sem effs (Maybe VScript)

-- | Gets the current reward associated with a credential
getCurrentReward ::
  ( Member MockChainRead effs,
    Script.ToCredential c
  ) =>
  c ->
  Sem effs (Maybe Api.Lovelace)
