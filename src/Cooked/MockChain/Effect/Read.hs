{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to query the current state of the
-- blockchain.
module Cooked.MockChain.Effect.Read
  ( -- * The `MockChainRead` effect
    MockChainRead,
    runMockChainRead,
    runMockChainReadNode,

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

    -- * Queries related to time
    currentSlot,
    currentMSRange,
    getEnclosingSlot,
    slotRangeBefore,
    slotRangeAfter,
    slotToMSRange,

    -- * Queries related to fetching UTxOs
    utxosAt,
    txSkelOutByRef,
    utxosFromCardanoTx,
    lookupUtxos,
    previewByRef,
    viewByRef,

    -- * Fetching reward amount query
    getCurrentReward,

    -- * The `MockChainReadExtra` effect
    MockChainReadExtra (..),
    runMockChainReadExtra,

    -- * Fetching all Utxos query
    allUtxos,

    -- * Retrieving the full constitution script query
    getConstitutionScript,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Core qualified as C.Ledger
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cardano.Slotting.Time qualified as Time
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.MockChain.Automation.GenerateTx.Credential (toStakeCredential)
import Cooked.MockChain.Common
import Cooked.MockChain.Runtime.Error
import Cooked.MockChain.Runtime.State
import Cooked.Skeleton
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Ledger.Slot qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers primitives to query the current state of the
-- mockchain. As its name suggests, this effect is read-only and does not alter
-- the state in any way.
data MockChainRead :: Effect where
  GetParams :: MockChainRead m (C.Ledger.PParams Conway.ConwayEra)
  TxSkelOutByRef :: Api.TxOutRef -> MockChainRead m TxSkelOut
  CurrentSlot :: MockChainRead m P.Ledger.Slot
  SlotToMSRange :: P.Ledger.Slot -> MockChainRead m (Api.POSIXTime, Api.POSIXTime)
  GetEnclosingSlot :: Api.POSIXTime -> MockChainRead m P.Ledger.Slot
  UtxosAt :: (Script.ToCredential a) => a -> MockChainRead m Utxos
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainRead m (Maybe Api.Lovelace)

makeSem_ ''MockChainRead

-- | The interpretation for read-only effect in the blockchain state
runMockChainRead ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainRead : effs) a ->
  Sem effs a
runMockChainRead = interpret $ \case
  GetParams -> gets $ Emulator.pEmulatorPParams . mcstParams
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . mcstOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ MCEUnknownOutRef oRef
  UtxosAt (Script.toCredential -> cred) -> fetchUtxos $ (== cred) . Script.toCredential
  CurrentSlot -> gets $ view $ mcstLedgerStateL % to Emulator.getSlot
  SlotToMSRange slot -> do
    slotConfig <- gets $ Emulator.pSlotConfig . mcstParams
    case Emulator.slotToPOSIXTimeRange slotConfig slot of
      Api.Interval
        (Api.LowerBound (Api.Finite l) leftclosed)
        (Api.UpperBound (Api.Finite r) rightclosed) ->
          return
            ( if leftclosed then l else l + 1,
              if rightclosed then r else r - 1
            )
      _ -> fail "Unexpected unbounded slot: please report a bug at https://github.com/tweag/cooked-validators/issues"
  GetEnclosingSlot t -> gets $ (`Emulator.posixTimeToEnclosingSlot` t) . Emulator.pSlotConfig . mcstParams
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
  Sem effs (C.Ledger.PParams Conway.ConwayEra)

-- | Retrieves the required governance action deposit amount
govActionDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
govActionDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppGovActionDepositL

-- | Retrieves the required drep deposit amount
dRepDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
dRepDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppDRepDepositL

-- | Retrieves the required stake address deposit amount
stakeAddressDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
stakeAddressDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppKeyDepositL

-- | Retrieves the required stake pool deposit amount
stakePoolDeposit ::
  (Member MockChainRead effs) =>
  Sem effs Api.Lovelace
stakePoolDeposit =
  getParams
    <&> Api.Lovelace
    . Cardano.unCoin
    . Lens.view Conway.ppPoolDepositL

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
  return $
    toListOf (txSkelRedeemedScriptsT % userVScriptL) txSkel
      <> txSkelSpendingScripts

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelInputScripts =
  fmap catMaybes
    . mapM (previewByRef (txSkelOutOwnerL % userVScriptAT))
    . Map.keys
    . txSkelInputs

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue ::
  (Member MockChainRead effs) =>
  TxSkel ->
  Sem effs Api.Value
txSkelInputValue =
  fmap mconcat
    . mapM (viewByRef txSkelOutValueL)
    . Map.keys
    . txSkelInputs

-- | Returns the current slot
currentSlot ::
  (Member MockChainRead effs) =>
  Sem effs P.Ledger.Slot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number.
slotToMSRange ::
  (Members '[MockChainRead, Fail] effs) =>
  P.Ledger.Slot ->
  Sem effs (Api.POSIXTime, Api.POSIXTime)

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
  Sem effs P.Ledger.Slot

-- | The infinite range of slots ending before or at the given time
slotRangeBefore ::
  (Members '[MockChainRead, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.SlotRange
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
  Sem effs P.Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToMSRange n
  return $ Api.from $ if t == a then n else n + 1

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member MockChainRead effs,
    Script.ToCredential cred
  ) =>
  cred ->
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
  P.Ledger.CardanoTx ->
  Sem effs [(Api.TxOutRef, TxSkelOut)]
utxosFromCardanoTx =
  mapM (\txOutRef -> (txOutRef,) <$> txSkelOutByRef txOutRef)
    . fmap (P.Ledger.fromCardanoTxIn . snd)
    . P.Ledger.getCardanoTxOutRefs

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

-- | Gets the current reward associated with a credential
getCurrentReward ::
  ( Member MockChainRead effs,
    Script.ToCredential c
  ) =>
  c ->
  Sem effs (Maybe Api.Lovelace)

data MockChainReadExtra :: Effect where
  AllUtxos :: MockChainReadExtra m Utxos
  GetConstitutionScript :: MockChainReadExtra m (Maybe VScript)

makeSem_ ''MockChainReadExtra

runMockChainReadExtra ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainReadExtra : effs) a ->
  Sem effs a
runMockChainReadExtra = interpret $ \case
  AllUtxos -> gets $ toListOf $ mcstOutputsL % to Map.toList % traversed % filtered (snd . snd) % to (fmap fst)
  GetConstitutionScript -> gets $ view mcstConstitutionL

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member MockChainReadExtra effs) =>
  Sem effs Utxos

-- | Gets the current official constitution script
getConstitutionScript ::
  (Member MockChainReadExtra effs) =>
  Sem effs (Maybe VScript)

-- * Interpreting `MockChainRead` against a deployed node

-- NOTE: The following is a first sketch of an interpretation of `MockChainRead`
-- against a real, deployed Cardano node, using `cardano-api`'s local-state
-- query and chain-sync protocols. The primitives that map directly onto
-- `cardano-api` queries are implemented; the ones that require rebuilding a
-- `TxSkelOut` from an on-chain output (as well as credential-based address
-- filtering and the exact credential conversion) are left as clearly marked
-- `TODO`s to be refined.

-- | Interpret the `MockChainRead` effect by talking to a deployed node through
-- a `Cardano.LocalNodeConnectInfo` (socket path and network id) provided via a
-- `Reader`, running in a stack featuring @IO@ (via `Embed`). Failures are
-- surfaced through the corresponding typed `Error` effects rather than being
-- collapsed into generic failures.
runMockChainReadNode ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Error Cardano.PastHorizonException,
         Error P.Ledger.ToCardanoError,
         Reader Cardano.LocalNodeConnectInfo
       ]
      effs
  ) =>
  Sem (MockChainRead : effs) a ->
  Sem effs a
runMockChainReadNode = interpret $ \case
  -- Protocol parameters: a plain shelley-based-era query.
  GetParams -> querySbe $ Cardano.queryProtocolParameters Cardano.ShelleyBasedEraConway
  -- The current slot is read from the chain tip.
  CurrentSlot -> ask >>= fmap chainTipSlot . embed . Cardano.getLocalChainTip
  -- Slot -> closed ms interval, computed from the era history and system start.
  SlotToMSRange slot -> do
    eraHistory <- execExpr Cardano.queryEraHistory >>= fromEither
    systemStart <- execExpr Cardano.querySystemStart >>= fromEither
    (relStart, slotLen) <- fromEither $ Cardano.getProgress (toSlotNo slot) eraHistory
    let startUTC = Time.fromRelativeTime systemStart relStart
        endUTC = addUTCTime (Time.getSlotLength slotLen) startUTC
    -- TODO: refine the closed-interval boundary handling (the emulator returns
    -- an inclusive ms interval; here we take [start, start + slotLength]).
    return (utcToPOSIXTime startUTC, utcToPOSIXTime endUTC)
  -- POSIXTime -> enclosing slot, via the era history interpreter.
  GetEnclosingSlot t -> do
    eraHistory <- execExpr Cardano.queryEraHistory >>= fromEither
    systemStart <- execExpr Cardano.querySystemStart >>= fromEither
    let relTime = Time.toRelativeTime systemStart (posixTimeToUTC t)
    fromSlotNo <$> fromEither (Cardano.getSlotForRelativeTime relTime eraHistory)
  -- All UTxOs owned by a credential.
  UtxosAt _cred -> do
    -- TODO: filter node-side by address. A credential alone does not determine
    -- an address (the staking part is unknown), and `QueryUTxOByAddress` takes
    -- full addresses. For now we query the whole set and would filter
    -- client-side by `Script.toCredential cred` once `txSkelOutFromApiTxOut` is
    -- implemented. Querying the whole UTxO set is expensive: refine later.
    utxo <- queryUtxos Cardano.QueryUTxOWhole
    mapM convertUtxo (Map.toList (Cardano.unUTxO utxo))
  -- A single output, resolved by its reference.
  TxSkelOutByRef oRef -> do
    txIn <- fromEither $ P.Ledger.toCardanoTxIn oRef
    utxo <- queryUtxos $ Cardano.QueryUTxOByTxIn $ Set.singleton txIn
    case Map.elems (Cardano.unUTxO utxo) of
      [txOut] -> txSkelOutFromApiTxOut txOut
      -- TODO: decide how a missing UTxO should be signalled by the node backend.
      _ -> error "runMockChainReadNode: TxSkelOutByRef on a missing UTxO"
  -- The current reward accumulated by a credential's stake address.
  GetCurrentReward (Script.toCredential -> cred) -> do
    networkId <- asks Cardano.localNodeNetworkId
    let stakeCred = toCardanoStakeCredential cred
        stakeAddr = Cardano.makeStakeAddress networkId stakeCred
    (rewards, _) <- querySbe $ Cardano.queryStakeAddresses Cardano.ShelleyBasedEraConway (Set.singleton stakeCred) networkId
    return $ Api.Lovelace . Cardano.unCoin <$> Map.lookup stakeAddr rewards
  where
    execExpr expr = ask >>= \conn -> embed (Cardano.executeLocalStateQueryExpr conn Cardano.VolatileTip expr) >>= fromEither
    querySbe expr = execExpr expr >>= fromEither >>= fromEither
    queryUtxos flt = querySbe (Cardano.queryUtxo Cardano.ShelleyBasedEraConway flt)
    chainTipSlot Cardano.ChainTipAtGenesis = P.Ledger.Slot 0
    chainTipSlot (Cardano.ChainTip slotNo _ _) = fromSlotNo slotNo
    toSlotNo = Cardano.SlotNo . fromInteger . P.Ledger.getSlot
    fromSlotNo (Cardano.SlotNo w) = P.Ledger.Slot (toInteger w)
    posixTimeToUTC t = posixSecondsToUTCTime (fromRational (toRational (Api.getPOSIXTime t) / 1000))
    utcToPOSIXTime u = Api.POSIXTime (round (1000 * utcTimeToPOSIXSeconds u))
    convertUtxo (txIn, txOut) = (P.Ledger.fromCardanoTxIn txIn,) <$> txSkelOutFromApiTxOut txOut
    -- TODO: reconstruct a `TxSkelOut` from an on-chain output (owner and staking
    -- credentials from the address, value, datum, reference script).
    txSkelOutFromApiTxOut _ = error "txSkelOutFromApiTxOut: not implemented yet"
    -- TODO: convert a Plutus credential into a `Cardano.StakeCredential`
    -- (`toStakeCredential`, already imported, may be reusable here).
    toCardanoStakeCredential _ = error "toCardanoStakeCredential: not implemented yet"
