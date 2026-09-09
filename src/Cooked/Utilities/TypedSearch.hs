-- | This module provides a set of utilities to perform and refine searches. In
-- particular, it allows to extract information from targetted elements and
-- filter them out. The idea is to start with a simple search and refine it with
-- filters while appending new elements to the list of extracted information.
module Cooked.Utilities.TypedSearch
  ( -- * Utxo searches types
    SearchResult,

    -- * Building a search result
    searchResultMapI,

    -- * Retrieving pieces of @SearchResult@
    retrieve,
    retrieveKeys,
    retrieveValues,
    retrieveByType,
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
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Optics.Core
import Optics.Core.Extras
import Witherable
import Prelude hiding (filter)

-- | A 'SearchResult' is a gathering of heterogeneous lists of extracted elements
-- within a given structure. Typically, this structure will be a map from a
-- given key type identifying the search results.
type SearchResult f els = f (HList els)

-- | An isomorphisms between a given map and a search results with a single
-- extracted element.
searchResultMapI :: (Functor f) => Iso' (SearchResult f '[v]) (f v)
searchResultMapI = iso (fmap hHead) (fmap hSingleton)

-- | Retrieves part of a 'SearchResult'. We define it on a more general type, to
-- allow for extracting values from basically anything, thus avoiding annoying
-- fmaps prepending sequences of search operators bound with @>>=@
retrieve ::
  (Applicative m) =>
  (res -> a) ->
  (res -> m a)
retrieve f = pure . f

-- | Retrieves the set of keys from a 'SearchResult' based on a map.
retrieveKeys ::
  (Applicative m) =>
  SearchResult (Map key) els ->
  m (Set key)
retrieveKeys = retrieve Map.keysSet

-- | Retrieve the extracted elements from a 'SearchResult'.
retrieveValues ::
  ( Applicative m,
    Foldable f
  ) =>
  SearchResult f els ->
  m [HList els]
retrieveValues = retrieve toList

-- | Retrieves the list of extracted elements of a given type from a
-- 'SearchResult', bound to their original keys.
retrieveByType ::
  ( FetchByType a els,
    Applicative m,
    Functor f
  ) =>
  SearchResult f els ->
  m (f a)
retrieveByType = retrieve $ fmap fetchByType

-- | Retrieves the list of extracted elements of a given type from a
-- 'SearchResult', no longer bound to their original keys.
retrieveByTypeAsList ::
  ( FetchByType a els,
    Applicative m,
    Functor f,
    Foldable f
  ) =>
  SearchResult f els ->
  m [a]
retrieveByTypeAsList = fmap toList . retrieveByType

-- | Extracts a new element from the currently selected outputs, filtering out
-- in the process utxos for which this element is not available
extract ::
  ( FetchByType a els,
    Applicative m,
    Witherable f
  ) =>
  (a -> m (Maybe b)) ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extract extractFun = wither $ \l -> fmap (`HCons` l) <$> extractFun (fetchByType l)

-- | Same as `extract`, but with a pure extraction function
extractPure ::
  ( FetchByType a els,
    Applicative m,
    Filterable f
  ) =>
  (a -> Maybe b) ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extractPure extractFun = pure . mapMaybe (\l -> (`HCons` l) <$> extractFun (fetchByType l))

-- | Same as `extractPure`, using an affine fold to extract the element
extractAFold ::
  ( FetchByType a els,
    Is k An_AffineFold,
    Applicative m,
    Filterable f
  ) =>
  Optic' k is a b ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extractAFold = extractPure . preview

-- | Same as `extract`, but with a total extraction function
extractTotal ::
  ( FetchByType a els,
    Applicative m,
    Witherable f
  ) =>
  (a -> m b) ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extractTotal = extract . (fmap Just .)

-- | Same as `extract`, but with a pure and total extraction function
extractPureTotal ::
  ( FetchByType a els,
    Applicative m,
    Functor f
  ) =>
  (a -> b) ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extractPureTotal extractFun = pure . fmap (\l -> HCons (extractFun (fetchByType l)) l)

-- | Same as `extractPureTotal`, using a getter to extract the element
extractGetter ::
  ( FetchByType a els,
    Is k A_Getter,
    Applicative m,
    Functor f
  ) =>
  Optic' k is a b ->
  SearchResult f els ->
  m (SearchResult f (b ': els))
extractGetter = extractPureTotal . view

-- | Ensures the outputs resulting from the search satisfy the given predicate
ensure ::
  ( FetchByType a els,
    Applicative m,
    Witherable f
  ) =>
  (a -> m Bool) ->
  SearchResult f els ->
  m (SearchResult f els)
ensure p = filterA (p . fetchByType)

-- | Same as `ensure`, but with a pure predicate
ensurePure ::
  ( FetchByType a els,
    Applicative m,
    Filterable f
  ) =>
  (a -> Bool) ->
  SearchResult f els ->
  m (SearchResult f els)
ensurePure filterFun = pure . filter (filterFun . fetchByType)

-- | Ensures the outputs resulting from the search contain the focus of the
-- given affine fold
ensureAFoldIs ::
  ( FetchByType a els,
    Is k An_AffineFold,
    Applicative m,
    Filterable f
  ) =>
  Optic' k is a b ->
  SearchResult f els ->
  m (SearchResult f els)
ensureAFoldIs = ensurePure . is

-- | Ensures the outputs resulting from the search do not contain the focus of
-- the given affine fold
ensureAFoldIsn't ::
  ( FetchByType a els,
    Is k An_AffineFold,
    Applicative m,
    Filterable f
  ) =>
  Optic' k is a b ->
  SearchResult f els ->
  m (SearchResult f els)
ensureAFoldIsn't = ensurePure . isn't
