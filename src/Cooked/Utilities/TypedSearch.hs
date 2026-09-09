-- | This module provides a set of utilities to perform and refine searches. In
-- particular, it allows to extract information from targetted elements and
-- filter them out. The idea is to start with a simple search and refine it with
-- filters while appending new elements to the list of extracted information.
module Cooked.Utilities.TypedSearch
  ( -- * Utxo searches types
    SearchResult,
    Search,

    -- * Building a search result
    searchResultMapI,

    -- * Retrieving pieces of @SearchResult@
    retrieve,
    retrieveKeys,
    retrieveValues,
    retrieveByTypeAsMap,
    retrieveByTypeAsList,

    -- * Extracting new information from existing extracted elements
    extract,
    extractPure,
    extractAFold,
    extractTotal,
    extractPureTotal,
    extractGetter,

    -- * Filtering some extracted elements
    ensure,
    ensurePure,
    ensureAFoldIs,
    ensureAFoldIsn't,
  )
where

import Cooked.Utilities.HList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Optics.Core
import Optics.Core.Extras
import Polysemy
import Witherable

-- | A 'SearchResult' is a map from a given key to an heterogeneous list of
-- extracted elements bound to this key.
type SearchResult key els = Map key (HList els)

-- | A 'Search' is a computation that returns a `SearchResult`.
type Search effs key els = Sem effs (SearchResult key els)

-- | An isomorphisms between a given map and a search results with a single
-- extracted element.
searchResultMapI :: Iso' (SearchResult key '[v]) (Map key v)
searchResultMapI = iso (fmap hHead) (fmap hSingleton)

-- | Retrieves part of a 'SearchResult'. We define it on a more general type, to
-- allow for extracting values from basically anything, thus avoiding annoying
-- fmaps prepending sequences of search operators bound with @>>=@
retrieve ::
  (els -> a) ->
  (els -> Sem effs a)
retrieve f = return . f

-- | Retrieves the set of keys from a 'SearchResult'
retrieveKeys ::
  SearchResult key els ->
  Sem effs (Set key)
retrieveKeys = retrieve Map.keysSet

-- | Retrieve the extracted elements from a 'SearchResult'.
retrieveValues ::
  SearchResult key els ->
  Sem effs [HList els]
retrieveValues = retrieve Map.elems

-- | Retrieves the list of extracted elements of a given type from a
-- 'SearchResult', bound to their original keys.
retrieveByTypeAsMap ::
  (FetchByType a els) =>
  SearchResult key els ->
  Sem effs (Map key a)
retrieveByTypeAsMap = retrieve $ fmap fetchByType

-- | Retrieves the list of extracted elements of a given type from a
-- 'SearchResult', no longer bound to their original keys.
retrieveByTypeAsList ::
  (FetchByType a els) =>
  SearchResult key els ->
  Sem effs [a]
retrieveByTypeAsList = fmap Map.elems . retrieveByTypeAsMap

-- | Extracts a new element from the currently selected outputs, filtering out
-- in the process utxos for which this element is not available
extract ::
  (FetchByType a els) =>
  (a -> Sem effs (Maybe b)) ->
  SearchResult key els ->
  Search effs key (b ': els)
extract extractFun = witherM $ \l -> fmap (`HCons` l) <$> extractFun (fetchByType l)

-- | Same as `extract`, but with a pure extraction function
extractPure ::
  (FetchByType a els) =>
  (a -> Maybe b) ->
  SearchResult key els ->
  Search effs key (b ': els)
extractPure = extract . (return .)

-- | Same as `extractPure`, using an affine fold to extract the element
extractAFold ::
  (FetchByType a els, Is k An_AffineFold) =>
  Optic' k is a b ->
  SearchResult key els ->
  Search effs key (b ': els)
extractAFold = extractPure . preview

-- | Same as `extract`, but with a total extraction function
extractTotal ::
  (FetchByType a els) =>
  (a -> Sem effs b) ->
  SearchResult key els ->
  Search effs key (b ': els)
extractTotal = extract . (fmap Just .)

-- | Same as `extract`, but with a pure and total extraction function
extractPureTotal ::
  (FetchByType a els) =>
  (a -> b) ->
  SearchResult key els ->
  Search effs key (b ': els)
extractPureTotal = extractTotal . (return .)

-- | Same as `extractPureTotal`, using a getter to extract the element
extractGetter ::
  (FetchByType a els, Is k A_Getter) =>
  Optic' k is a b ->
  SearchResult key els ->
  Search effs key (b ': els)
extractGetter = extractPureTotal . view

-- | Ensures the outputs resulting from the search satisfy the given predicate
ensure ::
  (FetchByType a els) =>
  (a -> Sem effs Bool) ->
  SearchResult key els ->
  Search effs key els
ensure p = filterA (p . fetchByType)

-- | Same as `ensure`, but with a pure predicate
ensurePure ::
  (FetchByType a els) =>
  (a -> Bool) ->
  SearchResult key els ->
  Search effs key els
ensurePure = ensure . (return .)

-- | Ensures the outputs resulting from the search contain the focus of the
-- given affine fold
ensureAFoldIs ::
  (FetchByType a els, Is k An_AffineFold) =>
  Optic' k is a b ->
  SearchResult key els ->
  Search effs key els
ensureAFoldIs = ensurePure . is

-- | Ensures the outputs resulting from the search do not contain the focus of
-- the given affine fold
ensureAFoldIsn't ::
  (FetchByType a els, Is k An_AffineFold) =>
  Optic' k is a b ->
  SearchResult key els ->
  Search effs key els
ensureAFoldIsn't = ensurePure . isn't
