-- | This module provides a convenient framework to look through UTxOs and:
-- - filter them in a convenient manner
-- - extract pieces of information from them
module Cooked.MockChain.UtxoSearch
  ( -- * UTxO searches
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

import Control.Monad (foldM)
import Cooked.Families hiding (Member)
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Read.Chain
import Cooked.Skeleton.Datum
import Cooked.Skeleton.Output
import Cooked.Skeleton.Value
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set
import Optics.Core
import Optics.Core.Extras
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Witherable

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
  (Member MockChainReadChain effs, Script.ToAddress pkh) =>
  pkh ->
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
utxosAtSearch pkh filters = filters $ beginSearch $ utxosAt pkh

-- | Searches for all the known utxos with a given filter
allUtxosSearch ::
  (Member MockChainReadChain effs) =>
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
allUtxosSearch filters = filters $ beginSearch allUtxos

-- | Searches for utxos belonging to a given list with a given filter
txSkelOutByRefSearch ::
  (Member MockChainReadChain effs) =>
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
  (Member MockChainReadChain effs) =>
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
