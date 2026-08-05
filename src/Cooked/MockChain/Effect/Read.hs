{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to query the current state of the
-- blockchain.
module Cooked.MockChain.Effect.Read
  ( -- * The 'MockChainRead' effect
    MockChainRead,

    -- * 'MockChainRead' interpreters
    runMockChainReadEmul,
    runMockChainReadNode,

    -- * Queries related to protocol parameters
    getParams,
    getNetworkId,
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
    getEraHistory,
    getSystemStart,
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

    -- * Query fetching the current reward amount
    getCurrentReward,

    -- * Query fetching the current full constitution script
    getConstitutionScript,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano hiding (TxIn)
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Core qualified as C.Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cardano.Slotting.Time qualified as Time
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.MockChain.Automation.GenerateTx.Credential
import Cooked.MockChain.Common
import Cooked.MockChain.Runtime.Error
import Cooked.MockChain.Runtime.State
import Cooked.Skeleton
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Set qualified as Set
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Ledger.Address qualified as P.Ledger
import Ledger.Slot qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
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
  GetNetworkId :: MockChainRead m Cardano.NetworkId
  TxSkelOutByRef :: Api.TxOutRef -> MockChainRead m TxSkelOut
  CurrentSlot :: MockChainRead m P.Ledger.Slot
  GetEraHistory :: MockChainRead m Cardano.EraHistory
  GetSystemStart :: MockChainRead m Time.SystemStart
  SlotToMSRange :: P.Ledger.Slot -> MockChainRead m (Api.POSIXTime, Api.POSIXTime)
  GetEnclosingSlot :: Api.POSIXTime -> MockChainRead m P.Ledger.Slot
  AllUtxos :: MockChainRead m Utxos
  UtxosAt :: (Script.ToAddress a) => a -> MockChainRead m Utxos
  GetConstitutionScript :: MockChainRead m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainRead m (Maybe Api.Lovelace)

makeSem_ ''MockChainRead

-- | The interpretation for read-only effect with a stored 'MockChainState'
runMockChainReadEmul ::
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
runMockChainReadEmul = interpret $ \case
  GetParams -> gets $ Emulator.pEmulatorPParams . mcstParams
  GetNetworkId -> gets $ Emulator.pNetworkId . mcstParams
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . mcstOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ MCEUnknownOutRef oRef
  AllUtxos -> fetchUtxos $ const True
  UtxosAt (Script.toAddress -> addr) -> fetchUtxos $ (== addr) . Script.toAddress
  CurrentSlot -> gets $ view $ mcstLedgerStateL % to Emulator.getSlot
  GetEraHistory -> gets $ Emulator.emulatorEraHistory . mcstParams
  GetSystemStart -> gets $ Shelley.systemStart . Emulator.emulatorGlobals . mcstParams
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
  Sem effs (C.Ledger.PParams Conway.ConwayEra)

-- | Returns the network id of the current chain
getNetworkId ::
  (Member MockChainRead effs) =>
  Sem effs Cardano.NetworkId

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

-- | Returns the era history of the chain, which notably allows converting slots
-- into epochs (see 'Cardano.slotToEpoch').
getEraHistory ::
  (Member MockChainRead effs) =>
  Sem effs Cardano.EraHistory

-- | Returns the system start time of the chain, that is the UTC time at which
-- the first slot begins.
getSystemStart ::
  (Member MockChainRead effs) =>
  Sem effs Time.SystemStart

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

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member MockChainRead effs) =>
  Sem effs Utxos

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member MockChainRead effs,
    Script.ToAddress cred
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

-- | Interpret the `MockChainRead` effect by talking to a deployed node through
-- a `Cardano.LocalNodeConnectInfo` (socket path and network id) provided via a
-- `Reader`, running in a stack featuring @IO@ (via `Embed`).
runMockChainReadNode ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Error Cardano.PastHorizonException,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Reader Cardano.LocalNodeConnectInfo
       ]
      effs
  ) =>
  Sem (MockChainRead : effs) a ->
  Sem effs a
runMockChainReadNode = interpret $ \case
  GetParams -> queryAndHandleErrors $ Cardano.queryProtocolParameters Cardano.ShelleyBasedEraConway
  GetNetworkId -> asks Cardano.localNodeNetworkId
  CurrentSlot -> ask >>= fmap chainTipSlot . embed . Cardano.getLocalChainTip
  GetEraHistory -> queryAndHandleError Cardano.queryEraHistory
  GetSystemStart -> queryAndHandleError Cardano.querySystemStart
  SlotToMSRange slot -> do
    eraHistory <- queryAndHandleError Cardano.queryEraHistory
    systemStart <- queryAndHandleError Cardano.querySystemStart
    (relStart, slotLen) <- fromEither $ Cardano.getProgress (toSlotNo slot) eraHistory
    let startUTC = Time.fromRelativeTime systemStart relStart
        endUTC = Time.getSlotLength slotLen `addUTCTime` startUTC
    return (utcToPOSIXTime startUTC, utcToPOSIXTime endUTC - 1)
  GetEnclosingSlot t -> do
    eraHistory <- queryAndHandleError Cardano.queryEraHistory
    systemStart <- queryAndHandleError Cardano.querySystemStart
    let relTime = Time.toRelativeTime systemStart $ posixTimeToUTC t
    fromSlotNo <$> fromEither (Cardano.getSlotForRelativeTime relTime eraHistory)
  AllUtxos -> queryUtxosAndHandleErrors Cardano.QueryUTxOWhole
  UtxosAt (Script.toAddress -> addr) -> do
    networkId <- asks Cardano.localNodeNetworkId
    (Cardano.AddressInEra _ cAddr) <- fromEither $ P.Ledger.toCardanoAddressInEra networkId addr
    queryUtxosAndHandleErrors $ Cardano.QueryUTxOByAddress $ Set.singleton $ Cardano.toAddressAny cAddr
  TxSkelOutByRef oRef -> do
    txIn <- fromEither $ P.Ledger.toCardanoTxIn oRef
    utxo <- queryUtxosAndHandleErrors $ Cardano.QueryUTxOByTxIn $ Set.singleton txIn
    case utxo of
      [(_, txSkelOut)] -> return txSkelOut
      -- This case is reduced to [] as there can never be more than one UTxO
      -- with a given 'Api.TxOutRef'.
      _ -> throw $ MCEUnknownOutRef oRef
  -- The constitution query only exposes the guardrail script /hash/, never the
  -- script bytes themselves. To recover the full script, we rely on the on-chain
  -- convention (used on the public networks) that the guardrail script is posted
  -- as a reference script at its own enterprise script address. We therefore
  -- derive that address from the queried hash, list the UTxOs sitting there, and
  -- return the reference script whose hash matches the constitution's. When no
  -- such reference script is present (e.g. on a private network where nobody
  -- posted it), we return 'Nothing'.
  GetConstitutionScript -> do
    Cardano.Constitution _ mScriptHash <-
      queryAndHandleErrors $ Cardano.queryConstitution Cardano.ConwayEraOnwardsConway
    case mScriptHash of
      SNothing -> return Nothing
      SJust (Cardano.ScriptHash -> scriptHash) -> do
        networkId <- asks Cardano.localNodeNetworkId
        utxo <-
          queryUtxosAndHandleErrors $
            Cardano.QueryUTxOByAddress $
              Set.singleton $
                Cardano.AddressShelley $
                  Cardano.makeShelleyAddress
                    networkId
                    (Cardano.PaymentCredentialByScript scriptHash)
                    Cardano.NoStakeAddress
        return $
          listToMaybe $
            [ script
            | (_, preview txSkelOutReferenceScriptAT -> Just script) <- utxo,
              Script.toScriptHash script == Script.toScriptHash scriptHash
            ]
  GetCurrentReward (Script.toCredential -> cred) -> do
    networkId <- asks Cardano.localNodeNetworkId
    stakeCred <- toStakeCredential cred
    (rewards, _) <-
      queryAndHandleErrors $
        Cardano.queryStakeAddresses
          Cardano.ShelleyBasedEraConway
          (Set.singleton (Cardano.fromShelleyStakeCredential stakeCred))
          networkId
    return $ Api.Lovelace . Cardano.unCoin <$> Map.lookup (Cardano.StakeAddress (Cardano.toShelleyNetwork networkId) stakeCred) rewards
  where
    -- Fetches the local node info, embeds a query in IO and handles errors
    query q = do
      conn <- ask
      response <- embed $ Cardano.executeLocalStateQueryExpr conn Cardano.VolatileTip q
      fromEither response
    -- Handles one more layer of errors from the response of a query
    queryAndHandleError q = query q >>= fromEither
    -- Handles a second layer of error from the response of a query
    queryAndHandleErrors q = queryAndHandleError q >>= fromEither
    -- Queries the Utxos present on-chain, handling the errors, and returns the
    -- query result in terms of @Utxos@
    queryUtxosAndHandleErrors utxoFilter = do
      utxo <- queryAndHandleErrors $ Cardano.queryUtxo Cardano.ShelleyBasedEraConway utxoFilter
      return $ bimap P.Ledger.fromCardanoTxIn convertUtxo <$> Map.toList (Cardano.unUTxO utxo)
    -- Retrieves the Plutus slot number from a chain tip
    chainTipSlot Cardano.ChainTipAtGenesis = P.Ledger.Slot 0
    chainTipSlot (Cardano.ChainTip slotNo _ _) = fromSlotNo slotNo
    -- Converts a Plutus slot to a Cardano slot
    toSlotNo = Cardano.SlotNo . fromInteger . P.Ledger.getSlot
    -- Converts a Cardano slot to a Plutus slot
    fromSlotNo (Cardano.SlotNo w) = P.Ledger.Slot (toInteger w)
    -- Converts a POSIX time to a UTC time
    posixTimeToUTC = posixSecondsToUTCTime . fromRational . (/ 1000) . toRational . Api.getPOSIXTime
    -- Converts a UTC time to a POSIX time
    utcToPOSIXTime = Api.POSIXTime . round . (1000 *) . utcTimeToPOSIXSeconds
    convertUtxo :: Cardano.TxOut Cardano.CtxUTxO Cardano.ConwayEra -> TxSkelOut
    convertUtxo (Cardano.TxOut (P.Ledger.toPlutusAddress -> (Api.Address cred stCred)) val dat refScript) =
      TxSkelOut
        (review userCredentialI cred)
        stCred
        ( dat & \case
            Cardano.TxOutDatumNone -> NoTxSkelOutDatum
            Cardano.TxOutDatumHash _ hash ->
              SomeTxSkelOutDatumHash $ Api.DatumHash $ Api.toBuiltin $ Cardano.serialiseToRawBytes hash
            Cardano.TxOutDatumInline _ datum ->
              SomeTxSkelOutDatum (P.Ledger.fromCardanoScriptData datum) Inline
        )
        (P.Ledger.fromCardanoValue $ P.Ledger.fromCardanoTxOutValue val)
        False
        (P.Ledger.fromCardanoReferenceScript refScript)
