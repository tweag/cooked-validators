-- | This module provides a convenient framework to look through UTxOs and:
-- - filter them in a convenient manner
-- - extract pieces of information from them
module Cooked.MockChain.UtxoSearch
  ( -- * UTxO searches
    UtxoSearch,
    beginSearch,

    -- * Processing search result
    UtxoSearchResult,
    getOutputs,
    getOutputsAndExtracts,
    getExtracts,
    getTxOutRefs,
    getTxOutRefsAndOutputs,

    -- * Basic UTxO searches
    utxosAtSearch,
    allUtxosSearch,
    txSkelOutByRefSearch,

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

import Control.Monad (filterM, forM)
import Cooked.Families hiding (Member)
import Cooked.MockChain.Common
import Cooked.MockChain.Read
import Cooked.Skeleton.Datum
import Cooked.Skeleton.Output
import Cooked.Skeleton.Value
import Data.Functor
import Data.Maybe
import Optics.Core
import Optics.Core.Extras
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

type UtxoSearchResult elems = [(Api.TxOutRef, HList (TxSkelOut ': elems))]

-- | A `UtxoSearch` is a computation that returns a list of UTxOs alongside
-- their `TxSkelOut` counterpart and a list of other elements retrieved from the
-- output. The idea is to begin with a simple search and refine the search with
-- filters while appending new elements to the list.
type UtxoSearch effs elems = Sem effs (UtxoSearchResult elems)

-- | Wraps up a computation returning a `Utxos` into a `UtxoSearch`
beginSearch ::
  Sem effs Utxos ->
  UtxoSearch effs '[]
beginSearch = fmap (fmap (fmap (`HCons` HEmpty)))

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult`
getOutputs ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [TxSkelOut]
getOutputs = fmap (fmap (hHead . snd))

-- | Retrieves the `TxSkelOut`s from a `UtxoSearchResult` alongside the
-- extracted elements
getOutputsAndExtracts ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [(TxSkelOut, HList elems)]
getOutputsAndExtracts =
  fmap (fmap (\(_, HCons output l) -> (output, l)))

-- | Retrieves the extracted elements from a `UtxoSearchResult`
getExtracts ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [HList elems]
getExtracts = fmap (fmap (hTail . snd))

-- | Retrieves the `Api.TxOutRef`s from a `UtxoSearchResult`
getTxOutRefs ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs [Api.TxOutRef]
getTxOutRefs = fmap (fmap fst)

-- | Retrieves both the `Api.TxOutRef`s and `TxSkelOut`s from a `UtxoSearchResult`
getTxOutRefsAndOutputs ::
  Sem effs (UtxoSearchResult elems) ->
  Sem effs Utxos
getTxOutRefsAndOutputs = fmap (fmap (\(oRef, HCons output _) -> (oRef, output)))

-- | Searches for utxos at a given address with a given filter
utxosAtSearch ::
  (Member MockChainRead effs, Script.ToCredential pkh) =>
  pkh ->
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
utxosAtSearch pkh filters = filters $ beginSearch $ utxosAt pkh

-- | Searches for all the known utxos with a given filter
allUtxosSearch ::
  (Member MockChainRead effs) =>
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
allUtxosSearch filters = filters $ beginSearch allUtxos

-- | Searches for utxos belonging to a given list
txSkelOutByRefSearch ::
  (Member MockChainRead effs) =>
  [Api.TxOutRef] ->
  (UtxoSearch effs '[] -> UtxoSearch effs els) ->
  UtxoSearch effs els
txSkelOutByRefSearch utxos filters =
  filters $ beginSearch (zip utxos <$> mapM txSkelOutByRef utxos)

-- | Extracts a new element from the currently selected outputs, filtering in
-- the process out utxos for which this element is not available
extract ::
  (TxSkelOut -> Sem effs (Maybe b)) ->
  UtxoSearch effs els ->
  UtxoSearch effs (b ': els)
extract extractFun comp = do
  resl <- comp
  resl' <- forM resl $
    \(oRef, HCons txSkelOut other) -> do
      res <- extractFun txSkelOut
      return $ res <&> (\x -> (oRef, HCons txSkelOut (HCons x other)))
  return $ catMaybes resl'

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
  comp >>= filterM (filterF . hHead . snd)

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
  ensureAFoldIsn't txSkelOutMReferenceScriptL
    . ensureAFoldIsn't txSkelOutMStakingCredentialL
    . ensureAFoldIsn't (txSkelOutDatumL % txSkelOutDatumKindAT)

-- | Same as 'onlyValueOutputsAtSearch', but also ensures the searched outputs
-- do not contain non-ADA assets.
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
