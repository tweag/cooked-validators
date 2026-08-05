-- | This module exposes the user-facing primitives to query the current state
-- of the blockchain, such as the available UTxOs, the current slot, and the
-- current constitution or rewards. The lower-level configuration primitives
-- (protocol parameters, network id, era history, system start) live in the
-- internal 'Cooked.MockChain.Effect.Read.Conf.MockChainReadConf' effect, which
-- this effect relies on during its own interpretation.
module Cooked.MockChain.Effect.Read.Chain
  ( -- * The 'MockChainReadChain' effect
    MockChainReadChain,

    -- * 'MockChainReadChain' interpreters
    runMockChainReadChainEmul,
    runMockChainReadChainNode,

    -- * Queries related to `Cooked.Skeleton.TxSkel`
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
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cardano.Slotting.Time qualified as Time
import Control.Monad
import Cooked.MockChain.Automation.GenerateTx.Credential
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Read.Conf
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
-- the state in any way. This is the user-facing read effect; its interpreters
-- rely on the internal
-- 'Cooked.MockChain.Effect.Read.Conf.MockChainReadConf' effect to resolve the
-- fixed chain configuration.
data MockChainReadChain :: Effect where
  TxSkelOutByRef :: Api.TxOutRef -> MockChainReadChain m TxSkelOut
  CurrentSlot :: MockChainReadChain m P.Ledger.Slot
  SlotToMSRange :: P.Ledger.Slot -> MockChainReadChain m (Api.POSIXTime, Api.POSIXTime)
  GetEnclosingSlot :: Api.POSIXTime -> MockChainReadChain m P.Ledger.Slot
  AllUtxos :: MockChainReadChain m Utxos
  UtxosAt :: (Script.ToAddress a) => a -> MockChainReadChain m Utxos
  GetConstitutionScript :: MockChainReadChain m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainReadChain m (Maybe Api.Lovelace)

makeSem_ ''MockChainReadChain

-- | Returns all scripts involved in this 'TxSkel'
txSkelAllScripts ::
  (Member MockChainReadChain effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelAllScripts txSkel = do
  txSkelSpendingScripts <- txSkelInputScripts txSkel
  return $
    toListOf (txSkelRedeemedScriptsT % userVScriptL) txSkel
      <> txSkelSpendingScripts

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts ::
  (Member MockChainReadChain effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelInputScripts =
  fmap catMaybes
    . mapM (previewByRef (txSkelOutOwnerL % userVScriptAT))
    . Map.keys
    . txSkelInputs

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue ::
  (Member MockChainReadChain effs) =>
  TxSkel ->
  Sem effs Api.Value
txSkelInputValue =
  fmap mconcat
    . mapM (viewByRef txSkelOutValueL)
    . Map.keys
    . txSkelInputs

-- | Returns the current slot
currentSlot ::
  (Member MockChainReadChain effs) =>
  Sem effs P.Ledger.Slot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number.
slotToMSRange ::
  (Members '[MockChainReadChain, Fail] effs) =>
  P.Ledger.Slot ->
  Sem effs (Api.POSIXTime, Api.POSIXTime)

-- | Returns the closed ms interval corresponding to the current slot
currentMSRange ::
  (Members '[MockChainReadChain, Fail] effs) =>
  Sem effs (Api.POSIXTime, Api.POSIXTime)
currentMSRange = slotToMSRange =<< currentSlot

-- | Return the slot that contains the given time. See 'slotToMSRange' for
-- some satisfied equational properties.
getEnclosingSlot ::
  (Member MockChainReadChain effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.Slot

-- | The infinite range of slots ending before or at the given time
slotRangeBefore ::
  (Members '[MockChainReadChain, Fail] effs) =>
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
  (Members '[MockChainReadChain, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToMSRange n
  return $ Api.from $ if t == a then n else n + 1

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member MockChainReadChain effs) =>
  Sem effs Utxos

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member MockChainReadChain effs,
    Script.ToAddress cred
  ) =>
  cred ->
  Sem effs Utxos

-- | Returns an output given a reference to it
txSkelOutByRef ::
  (Member MockChainReadChain effs) =>
  Api.TxOutRef ->
  Sem effs TxSkelOut

-- | Retrieves the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx ::
  (Member MockChainReadChain effs) =>
  P.Ledger.CardanoTx ->
  Sem effs [(Api.TxOutRef, TxSkelOut)]
utxosFromCardanoTx =
  mapM (\txOutRef -> (txOutRef,) <$> txSkelOutByRef txOutRef)
    . fmap (P.Ledger.fromCardanoTxIn . snd)
    . P.Ledger.getCardanoTxOutRefs

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
lookupUtxos ::
  (Member MockChainReadChain effs) =>
  [Api.TxOutRef] ->
  Sem effs (Map Api.TxOutRef TxSkelOut)
lookupUtxos =
  foldM
    (\m oRef -> flip (Map.insert oRef) m <$> txSkelOutByRef oRef)
    Map.empty

-- | Retrieves an output and views a specific element out of it
viewByRef ::
  ( Member MockChainReadChain effs,
    Is g A_Getter
  ) =>
  Optic' g is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs c
viewByRef optic = (view optic <$>) . txSkelOutByRef

-- | Retrieves an output and previews a specific element out of it
previewByRef ::
  ( Member MockChainReadChain effs,
    Is af An_AffineFold
  ) =>
  Optic' af is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs (Maybe c)
previewByRef optic = (preview optic <$>) . txSkelOutByRef

-- | Gets the current official constitution script
getConstitutionScript ::
  (Member MockChainReadChain effs) =>
  Sem effs (Maybe VScript)

-- | Gets the current reward associated with a credential
getCurrentReward ::
  ( Member MockChainReadChain effs,
    Script.ToCredential c
  ) =>
  c ->
  Sem effs (Maybe Api.Lovelace)

-- | The interpretation for read-only effect with a stored 'EmulatorState' and
-- 'ChainIndex'
runMockChainReadChainEmul ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainReadChain : effs) a ->
  Sem effs a
runMockChainReadChainEmul = interpret $ \case
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . chainIndexOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ MCEUnknownOutRef oRef
  AllUtxos -> fetchUtxos $ const True
  UtxosAt (Script.toAddress -> addr) -> fetchUtxos $ (== addr) . Script.toAddress
  CurrentSlot -> gets $ view $ emulatorStateLedgerStateL % to Emulator.getSlot
  SlotToMSRange slot -> do
    slotConfig <- gets $ Emulator.pSlotConfig . emulatorStateParams
    case Emulator.slotToPOSIXTimeRange slotConfig slot of
      Api.Interval
        (Api.LowerBound (Api.Finite l) leftclosed)
        (Api.UpperBound (Api.Finite r) rightclosed) ->
          return
            ( if leftclosed then l else l + 1,
              if rightclosed then r else r - 1
            )
      _ -> fail "Unexpected unbounded slot: please report a bug at https://github.com/tweag/cooked-validators/issues"
  GetEnclosingSlot t -> gets $ (`Emulator.posixTimeToEnclosingSlot` t) . Emulator.pSlotConfig . emulatorStateParams
  GetConstitutionScript -> gets $ view chainIndexConstitutionL
  GetCurrentReward (Script.toCredential -> cred) -> do
    stakeCredential <- toStakeCredential cred
    gets $
      preview $
        emulatorStateLedgerStateL
          % to (Emulator.getReward stakeCredential)
          % _Just
          % to coerce
  where
    fetchUtxos decide =
      gets $
        toListOf $
          chainIndexOutputsL
            % to Map.toList
            % traversed
            % filtered (snd . snd)
            % filtered (decide . fst . snd)
            % to (fmap fst)

-- | Interpret the `MockChainReadChain` effect by talking to a deployed node
-- through a `Cardano.LocalNodeConnectInfo` (socket path and network id)
-- provided via a `Reader`, running in a stack featuring @IO@ (via `Embed`). The
-- fixed chain configuration is resolved through the internal
-- 'Cooked.MockChain.Effect.Read.Conf.MockChainReadConf' effect.
runMockChainReadChainNode ::
  forall effs a.
  ( Members
      '[ Embed IO,
         MockChainReadConf,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Error Cardano.PastHorizonException,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Reader Cardano.LocalNodeConnectInfo,
         State ChainIndex
       ]
      effs
  ) =>
  Sem (MockChainReadChain : effs) a ->
  Sem effs a
runMockChainReadChainNode = interpret $ \case
  CurrentSlot -> ask >>= fmap chainTipSlot . embed . Cardano.getLocalChainTip
  SlotToMSRange slot -> do
    eraHistory <- getEraHistory
    systemStart <- getSystemStart
    (relStart, slotLen) <- fromEither $ Cardano.getProgress (toSlotNo slot) eraHistory
    let startUTC = Time.fromRelativeTime systemStart relStart
        endUTC = Time.getSlotLength slotLen `addUTCTime` startUTC
    return (utcToPOSIXTime startUTC, utcToPOSIXTime endUTC - 1)
  GetEnclosingSlot t -> do
    eraHistory <- getEraHistory
    systemStart <- getSystemStart
    let relTime = Time.toRelativeTime systemStart $ posixTimeToUTC t
    fromSlotNo <$> fromEither (Cardano.getSlotForRelativeTime relTime eraHistory)
  AllUtxos -> queryUtxosAndHandleErrors Cardano.QueryUTxOWhole
  UtxosAt (Script.toAddress -> addr) -> do
    networkId <- getNetworkId
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
  GetConstitutionScript -> do
    -- We retrieve the official optional script hash of the current constitution
    Cardano.Constitution _ mScriptHash <-
      queryAndHandleErrors $ Cardano.queryConstitution Cardano.ConwayEraOnwardsConway
    -- We retrieve the optional constitution already stored in the chain index
    mStoredConstitution <- gets chainIndexConstitution
    -- We inspect the current option constitution script hash
    case mScriptHash of
      -- There is no official constitution (should not happen). We just set our
      -- own constitution to @Nothing@ accordingly.
      SNothing -> do
        modify' $ set chainIndexConstitutionL Nothing
        return Nothing
      -- There is an official constitution, and it matches the stored one, which
      -- we directly return.
      SJust (Cardano.ScriptHash -> scriptHash)
        | Just storedConstitution <- mStoredConstitution,
          Script.toScriptHash scriptHash == Script.toScriptHash storedConstitution ->
            return $ Just storedConstitution
      -- There is an official constitution, and it does not match the stored one
      -- (it has changed, or it's the first time it's been queried). We fetch
      -- the actual constitution from a reference script at its own address,
      -- where it should live, according to a governance convention. We store
      -- the script we find there after verifying its hash, and return it.
      SJust (Cardano.ScriptHash -> scriptHash) -> do
        networkId <- getNetworkId
        utxo <-
          queryUtxosAndHandleErrors $
            Cardano.QueryUTxOByAddress $
              Set.singleton $
                Cardano.AddressShelley $
                  Cardano.makeShelleyAddress
                    networkId
                    (Cardano.PaymentCredentialByScript scriptHash)
                    Cardano.NoStakeAddress
        let newConstitution =
              listToMaybe $
                [ script
                | (_, preview txSkelOutReferenceScriptAT -> Just script) <- utxo,
                  Script.toScriptHash script == Script.toScriptHash scriptHash
                ]
        modify' $ set chainIndexConstitutionL newConstitution
        return newConstitution
  GetCurrentReward (Script.toCredential -> cred) -> do
    networkId <- getNetworkId
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
