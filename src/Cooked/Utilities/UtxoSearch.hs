-- | This module provides a set of utilities to perform searches on UTxOs, in
-- particular to extract information from them and filter them out. The idea is
-- to start with a simple search and refine it with filters while appending new
-- elements to the list of extracted information.
module Cooked.Utilities.UtxoSearch
  ( -- * Utxo searches types
    RefinedOutputsList,
    UtxoSearchResult,
    utxosSearchResultUtxosI,

    -- * Retrieving pieces of @UtxoSearchResult@
    retrieve,
    retrieveUtxos,
    retrieveRefinedOutputs,
    retrieveExtracts,
    retrieveTxOutRefs,
    retrieveExtractedHeads,

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
  )
where

import Cooked.Skeleton.Output
import Cooked.Utilities.Aliases
import Cooked.Utilities.Families
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Optics.Core
import Optics.Core.Extras
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Witherable

-- | An heterogeneous list starting with a 'TxSkelOut', followed by any element
-- that was extracted during the search.
type RefinedOutputsList els = HList (TxSkelOut ': els)

-- | Raw result of a `UtxoSearch`. We store the `Api.TxOutRef` of the output,
-- alongside an heterogeneous list starting with the output in question,
-- followed by any element that was extracted during the search.
type UtxoSearchResult els = Map Api.TxOutRef (RefinedOutputsList els)

-- | An isomorphisms between `Utxos` and search results with no extra element.
utxosSearchResultUtxosI :: Iso' (UtxoSearchResult '[]) Utxos
utxosSearchResultUtxosI = iso (fmap hHead) (fmap hSingleton)

-- | A `UtxoSearch` is a computation that returns a list of UTxOs alongside
-- their `TxSkelOut` counterpart and a list of other elements retrieved from the
-- output. The idea is to begin with a simple search and refine the search with
-- filters while appending new elements to the list.
type UtxoSearch effs els = Sem effs (UtxoSearchResult els)

-- | Retrieves part of a 'UtxoSearchResult'. We define it on a more general
-- type, to allow for extracting values from basically anything, thus avoiding
-- annoying fmaps prepending sequences of utxo search operators bound with @>>=@
retrieve ::
  (els -> a) ->
  (els -> Sem effs a)
retrieve f = return . f

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult`
retrieveUtxos ::
  UtxoSearchResult els ->
  Sem effs Utxos
retrieveUtxos = retrieve $ fmap hHead

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult` alongside the
-- extracted elements
retrieveRefinedOutputs ::
  UtxoSearchResult els ->
  Sem effs [RefinedOutputsList els]
retrieveRefinedOutputs = retrieve Map.elems

-- | Retrieves the extracted elements from a `UtxoSearchResult`
retrieveExtracts ::
  UtxoSearchResult els ->
  Sem effs [HList els]
retrieveExtracts = retrieve $ Map.elems . fmap hTail

-- | Retrieves the `Api.TxOutRef`s from a `UtxoSearchResult`
retrieveTxOutRefs ::
  UtxoSearchResult els ->
  Sem effs (Set Api.TxOutRef)
retrieveTxOutRefs = retrieve Map.keysSet

-- | Retrieves the first extracted elements from a 'UtxoSearchResult'
retrieveExtractedHeads ::
  UtxoSearchResult (a ': els) ->
  Sem effs [a]
retrieveExtractedHeads = retrieve $ Map.elems . fmap (hHead . hTail)

-- | Extracts a new element from the currently selected outputs, filtering out
-- in the process utxos for which this element is not available
extract ::
  (TxSkelOut -> Sem effs (Maybe b)) ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extract extractFun =
  witherM
    ( \(HCons txSkelOut es) ->
        fmap (HCons txSkelOut . (`HCons` es)) <$> extractFun txSkelOut
    )

-- | Same as `extract`, but with a pure extraction function
extractPure ::
  (TxSkelOut -> Maybe b) ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extractPure = extract . (return .)

-- | Same as `extractPure`, using an affine fold to extract the element
extractAFold ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extractAFold = extractPure . preview

-- | Same as `extract`, but with a total extraction function
extractTotal ::
  (TxSkelOut -> Sem effs b) ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extractTotal = extract . (fmap Just .)

-- | Same as `extract`, but with a pure and total extraction function
extractPureTotal ::
  (TxSkelOut -> b) ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extractPureTotal = extractTotal . (return .)

-- | Same as `extractPureTotal`, using a getter to extract the element
extractGetter ::
  (Is k A_Getter) =>
  Optic' k is TxSkelOut b ->
  UtxoSearchResult els ->
  UtxoSearch effs (b ': els)
extractGetter = extractPureTotal . view

-- | Ensures a specific element resulting from the search satisfy the given predicate
ensureSelecting ::
  (RefinedOutputsList els -> a) ->
  (a -> Sem effs Bool) ->
  UtxoSearchResult els ->
  UtxoSearch effs els
ensureSelecting f p = filterA (p . f)

-- | Ensures the outputs resulting from the search satisfy the given predicate
ensure ::
  (TxSkelOut -> Sem effs Bool) ->
  UtxoSearchResult els ->
  UtxoSearch effs els
ensure = ensureSelecting hHead

-- | Same as `ensure`, but with a pure predicate
ensurePure ::
  (TxSkelOut -> Bool) ->
  UtxoSearchResult els ->
  UtxoSearch effs els
ensurePure = ensure . (return .)

-- | Ensures the outputs resulting from the search contain the focus of the
-- given affine fold
ensureAFoldIs ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearchResult els ->
  UtxoSearch effs els
ensureAFoldIs = ensurePure . is

-- | Ensures the outputs resulting from the search do not contain the focus of
-- the given affine fold
ensureAFoldIsn't ::
  (Is k An_AffineFold) =>
  Optic' k is TxSkelOut b ->
  UtxoSearchResult els ->
  UtxoSearch effs els
ensureAFoldIsn't = ensurePure . isn't
