-- | This module exposes the user-facing primitives to query the current state
-- of the blockchain, such as the available UTxOs, and the current constitution
-- or rewards. It also provides the 'UtxoSearch' framework, a convenient way to
-- look through UTxOs, filter them, and extract pieces of information from them.
-- Time-related queries live in the separate
-- 'Cooked.Effect.Time.Time' effect. The lower-level
-- configuration primitives (protocol parameters, network id, era history, system
-- start) live in the internal
-- 'Cooked.Effect.Params.Params' effect, which this
-- effect relies on during its own interpretation.
module Cooked.Effect.Query
  ( -- * The 'Query' effect
    Query,

    -- * 'Query' interpreters
    runMockChainQuery,
    runBlockChainQuery,

    -- * Queries related to `Cooked.Skeleton.TxSkel`
    txSkelAllScripts,
    txSkelInputScripts,
    txSkelInputValue,

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

    -- * UTxO searches
    UtxoSearch,
    beginSearch,
    beginSearchPure,

    -- * Processing search result
    RefinedOutputsList,
    UtxoSearchResult,
    utxosSearchResultUtxosI,
    getUtxos,
    getOutputsAndExtracts,
    getExtracts,
    getTxOutRefs,

    -- * Basic UTxO searches
    utxosAtSearch,
    allUtxosSearch,
    txSkelOutByRefSearch,
    txSkelOutByRefSearch',

    -- * Extracting new information from UTxOs
    extract,
    extractPure,
    extractAFold,
    extractTotal,
    extractPureTotal,
    extractGetter,

    -- * Filtering some UTxOs out
    ensure,
    ensurePure,
    ensureAFoldIs,
    ensureAFoldIsn't,

    -- * Cooked filters
    ensureOnlyValueOutputs,
    ensureVanillaOutputs,
    ensureProperReferenceScript,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano hiding (TxIn)
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.Automation.GenerateTx.Credential
import Cooked.Effect.Params
import Cooked.Runtime.Error
import Cooked.Runtime.State
import Cooked.Skeleton
import Cooked.Utilities.Aliases
import Cooked.Utilities.Families hiding (Member)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Optics (toMapOf)
import Data.Maybe
import Data.Maybe.Strict
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Address qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Optics.Core.Extras
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Witherable (filterA, witherM)

-- | An effect that offers primitives to query the current state of the
-- mockchain. As its name suggests, this effect is read-only and does not alter
-- the state in any way. This is the user-facing read effect; its interpreters
-- rely on the internal
-- 'Cooked.Effect.Params.Params' effect to resolve the
-- fixed chain configuration.
data Query :: Effect where
  TxSkelOutByRef :: Api.TxOutRef -> Query m TxSkelOut
  AllUtxos :: Query m Utxos
  UtxosAt :: (Script.ToAddress a) => a -> Query m Utxos
  GetConstitutionScript :: Query m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> Query m (Maybe Api.Lovelace)

makeSem_ ''Query

-- | Returns all scripts involved in this 'TxSkel'
txSkelAllScripts ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelAllScripts txSkel = do
  txSkelSpendingScripts <- txSkelInputScripts txSkel
  return $
    toListOf (txSkelRedeemedScriptsT % userVScriptL) txSkel
      <> txSkelSpendingScripts

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelInputScripts =
  fmap catMaybes
    . mapM (previewByRef (txSkelOutOwnerL % userVScriptAT))
    . Map.keys
    . txSkelInputs

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs Api.Value
txSkelInputValue =
  fmap mconcat
    . mapM (viewByRef txSkelOutValueL)
    . Map.keys
    . txSkelInputs

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member Query effs) =>
  Sem effs Utxos

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member Query effs,
    Script.ToAddress cred
  ) =>
  cred ->
  Sem effs Utxos

-- | Returns an output given a reference to it
txSkelOutByRef ::
  (Member Query effs) =>
  Api.TxOutRef ->
  Sem effs TxSkelOut

-- | Retrieves the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx ::
  (Member Query effs) =>
  P.Ledger.CardanoTx ->
  Sem effs [(Api.TxOutRef, TxSkelOut)]
utxosFromCardanoTx =
  mapM (\txOutRef -> (txOutRef,) <$> txSkelOutByRef txOutRef)
    . fmap (P.Ledger.fromCardanoTxIn . snd)
    . P.Ledger.getCardanoTxOutRefs

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
lookupUtxos ::
  (Member Query effs) =>
  [Api.TxOutRef] ->
  Sem effs (Map Api.TxOutRef TxSkelOut)
lookupUtxos =
  foldM
    (\m oRef -> flip (Map.insert oRef) m <$> txSkelOutByRef oRef)
    Map.empty

-- | Retrieves an output and views a specific element out of it
viewByRef ::
  ( Member Query effs,
    Is g A_Getter
  ) =>
  Optic' g is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs c
viewByRef optic = (view optic <$>) . txSkelOutByRef

-- | Retrieves an output and previews a specific element out of it
previewByRef ::
  ( Member Query effs,
    Is af An_AffineFold
  ) =>
  Optic' af is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs (Maybe c)
previewByRef optic = (preview optic <$>) . txSkelOutByRef

-- | Gets the current official constitution script
getConstitutionScript ::
  (Member Query effs) =>
  Sem effs (Maybe VScript)

-- | Gets the current reward associated with a credential
getCurrentReward ::
  ( Member Query effs,
    Script.ToCredential c
  ) =>
  c ->
  Sem effs (Maybe Api.Lovelace)

-- | The interpretation for read-only effect with a stored 'EmulatorState' and
-- 'ChainIndex'
runMockChainQuery ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error ChainError
       ]
      effs
  ) =>
  Sem (Query : effs) a ->
  Sem effs a
runMockChainQuery = interpret $ \case
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . chainIndexOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ CEUnknownOutRef oRef
  AllUtxos -> fetchUtxos $ const True
  UtxosAt (Script.toAddress -> addr) -> fetchUtxos $ (== addr) . Script.toAddress
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
        toMapOf $
          chainIndexOutputsL
            % itraversed
            % filtered snd
            % filtered (decide . fst)
            % to fst

-- | Interpret the `Query` effect by talking to a deployed node
-- through a `Cardano.LocalNodeConnectInfo` (socket path and network id)
-- provided via a `Reader`, running in a stack featuring @IO@ (via `Embed`). The
-- fixed chain configuration is resolved through the internal
-- 'Cooked.Effect.Params.Params' effect.
runBlockChainQuery ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Params,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Error P.Ledger.ToCardanoError,
         Error ChainError,
         Reader Cardano.LocalNodeConnectInfo,
         State ChainIndex
       ]
      effs
  ) =>
  Sem (Query : effs) a ->
  Sem effs a
runBlockChainQuery = interpret $ \case
  AllUtxos -> queryUtxosAndHandleErrors Cardano.QueryUTxOWhole
  UtxosAt (Script.toAddress -> addr) -> do
    networkId <- getNetworkId
    (Cardano.AddressInEra _ cAddr) <- fromEither $ P.Ledger.toCardanoAddressInEra networkId addr
    queryUtxosAndHandleErrors $ Cardano.QueryUTxOByAddress $ Set.singleton $ Cardano.toAddressAny cAddr
  TxSkelOutByRef oRef -> do
    txIn <- fromEither $ P.Ledger.toCardanoTxIn oRef
    utxo <- queryUtxosAndHandleErrors $ Cardano.QueryUTxOByTxIn $ Set.singleton txIn
    maybe (throw $ CEUnknownOutRef oRef) return $ Map.lookup oRef utxo
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
                | (_, preview txSkelOutReferenceScriptAT -> Just script) <- Map.toList utxo,
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
    -- query result in terms of @Utxos@, updated with the known chain index.
    queryUtxosAndHandleErrors utxoFilter = do
      utxo <- queryAndHandleErrors $ Cardano.queryUtxo Cardano.ShelleyBasedEraConway utxoFilter
      knownUtxos <- gets chainIndexOutputs
      return $
        Map.mapWithKey
          (\oRef txSkelOut -> maybe txSkelOut fst $ Map.lookup oRef knownUtxos)
          (Map.mapKeysMonotonic P.Ledger.fromCardanoTxIn $ convertUtxo <$> Cardano.unUTxO utxo)
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

-- | An heterogeneous list starting with a 'TxSkelOut'
type RefinedOutputsList elems = HList (TxSkelOut ': elems)

-- | Raw result of a `UtxoSearch`. We store the `Api.TxOutRef` of the output,
-- alongside an heterogeneous list starting with the output in question,
-- followed by any element that was extracted during the search.
type UtxoSearchResult elems = Map Api.TxOutRef (RefinedOutputsList elems)

-- | An isomorphisms between `Utxos` and search results with no extra element.
utxosSearchResultUtxosI :: Iso' (UtxoSearchResult '[]) Utxos
utxosSearchResultUtxosI = iso (fmap hHead) (fmap hSingleton)

-- | A `UtxoSearch` is a computation that returns a list of UTxOs alongside
-- their `TxSkelOut` counterpart and a list of other elements retrieved from the
-- output. The idea is to begin with a simple search and refine the search with
-- filters while appending new elements to the list.
type UtxoSearch effs elems = Sem effs (UtxoSearchResult elems)

-- | Wraps up a computation returning a `Utxos` into a `UtxoSearch`
beginSearch ::
  Sem effs Utxos ->
  UtxoSearch effs '[]
beginSearch = fmap $ review utxosSearchResultUtxosI

-- | Same as `beginSearch` with a pure input
beginSearchPure ::
  Utxos ->
  UtxoSearch effs '[]
beginSearchPure = beginSearch . return

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult`
getUtxos ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs Utxos
getUtxos = fmap (fmap hHead)

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult` alongside the
-- extracted elements
getOutputsAndExtracts ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [RefinedOutputsList elems]
getOutputsAndExtracts = fmap Map.elems

-- | Retrieves the extracted elements from a `UtxoSearchResult`
getExtracts ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [HList elems]
getExtracts = fmap (Map.elems . fmap hTail)

-- | Retrieves the `Api.TxOutRef`s from a `UtxoSearchResult`
getTxOutRefs ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs (Set Api.TxOutRef)
getTxOutRefs = fmap Map.keysSet

-- | Searches for utxos at a given address with a given filter
utxosAtSearch ::
  (Member Query effs, Script.ToAddress pkh) =>
  pkh ->
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
utxosAtSearch pkh filters = filters $ beginSearch $ utxosAt pkh

-- | Searches for all the known utxos with a given filter
allUtxosSearch ::
  (Member Query effs) =>
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
allUtxosSearch filters = filters $ beginSearch allUtxos

-- | Searches for utxos belonging to a given list with a given filter
txSkelOutByRefSearch ::
  (Member Query effs) =>
  Set Api.TxOutRef ->
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
txSkelOutByRefSearch utxos filters =
  filters $
    foldM
      (\acc oRef -> (\x -> Map.insert oRef (hSingleton x) acc) <$> txSkelOutByRef oRef)
      Map.empty
      utxos

-- | Searches for utxos belonging to a given list with no filter
txSkelOutByRefSearch' ::
  (Member Query effs) =>
  Set Api.TxOutRef ->
  UtxoSearch effs '[]
txSkelOutByRefSearch' = (`txSkelOutByRefSearch` id)

-- | Extracts a new element from the currently selected outputs, filtering out
-- in the process utxos for which this element is not available
extract ::
  (TxSkelOut -> Sem effs (Maybe b)) ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extract extractFun =
  (>>= witherM (\(HCons txSkelOut es) -> fmap (HCons txSkelOut . (`HCons` es)) <$> extractFun txSkelOut))

-- | Same as `extract`, but with a pure extraction function
extractPure ::
  (TxSkelOut -> Maybe b) ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extractPure = extract . (return .)

-- | Same as `extractPure`, using an affine fold to extract the element
extractAFold ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extractAFold = extractPure . preview

-- | Same as `extract`, but with a total extraction function
extractTotal ::
  (TxSkelOut -> Sem effs b) ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extractTotal = extract . (fmap Just .)

-- | Same as `extract`, but with a pure and total extraction function
extractPureTotal ::
  (TxSkelOut -> b) ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extractPureTotal = extractTotal . (return .)

-- | Same as `extractPureTotal`, using a getter to extract the element
extractGetter ::
  (Is k A_Getter) =>
  Optic' k is TxSkelOut b ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extractGetter = extractPureTotal . view

-- | Ensures the outputs resulting from the search satisfy the given predicate
ensure ::
  (TxSkelOut -> Sem effs Bool) ->
  UtxoSearch effs els ->
  UtxoSearch effs els
ensure filterF comp =
  comp >>= filterA (filterF . hHead)

-- | Same as `ensure`, but with a pure predicate
ensurePure ::
  (TxSkelOut -> Bool) ->
  UtxoSearch effs els ->
  UtxoSearch effs els
ensurePure = ensure . (return .)

-- | Ensures the outputs resulting from the search contain the focus of the
-- given affine fold
ensureAFoldIs ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearch effs els ->
  UtxoSearch effs els
ensureAFoldIs = ensurePure . is

-- | Ensures the outputs resulting from the search do not contain the focus of
-- the given affine fold
ensureAFoldIsn't ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearch effs els ->
  UtxoSearch effs els
ensureAFoldIsn't = ensurePure . isn't

-- | Ensures the outputs resulting from the search do not have a reference
-- script, nor a staking credential, nor a datum
ensureOnlyValueOutputs ::
  UtxoSearch effs els ->
  UtxoSearch effs els
ensureOnlyValueOutputs =
  ensureAFoldIsn't txSkelOutReferenceScriptAT
    . ensureAFoldIsn't txSkelOutStakingCredentialAT
    . ensureAFoldIsn't (txSkelOutDatumL % txSkelOutDatumKindAT)

-- | Same as 'ensureOnlyValueOutputs', but also ensures the searched outputs do not
-- contain non-ADA assets.
ensureVanillaOutputs ::
  UtxoSearch effs els ->
  UtxoSearch effs els
ensureVanillaOutputs =
  ensureAFoldIs (txSkelOutValueL % valueLovelaceP)
    . ensureOnlyValueOutputs

-- | Ensures the outputs resulting from the search have the given script as a
-- reference script
ensureProperReferenceScript ::
  (Script.ToScriptHash s) =>
  s ->
  UtxoSearch effs els ->
  UtxoSearch effs els
ensureProperReferenceScript (Script.toScriptHash -> sHash) =
  ensureAFoldIs (txSkelOutReferenceScriptHashAF % filtered (== sHash))
