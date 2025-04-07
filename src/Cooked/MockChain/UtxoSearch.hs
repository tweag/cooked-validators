-- | This module provides a convenient framework to look through UTxOs and
-- search relevant ones based on predicates. For instance, it makes it very
-- convenient to gather all UTxOs at a certain address.
module Cooked.MockChain.UtxoSearch
  ( UtxoSearch,
    runUtxoSearch,
    allUtxosSearch,
    utxosAtSearch,
    utxosFromCardanoTxSearch,
    txOutByRefSearch,
    filterWith,
    filterWithPure,
    filterWithOptic,
    filterWithPred,
    filterWithValuePred,
    filterWithOnlyAda,
    filterWithNotOnlyAda,
    onlyValueOutputsAtSearch,
    vanillaOutputsAtSearch,
    filterWithAlways,
    scriptOutputsSearch,
    referenceScriptOutputsSearch,
  )
where

import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.Output
import Data.Maybe
import Ledger.Tx qualified as Ledger
import ListT (ListT (..))
import ListT qualified
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api

-- * The type of UTxO searches

-- | If a UTxO is a 'Api.TxOutRef' with some additional information, this type
-- captures a "stream" of UTxOs.
type UtxoSearch m a = ListT m (Api.TxOutRef, a)

-- | Given a UTxO search, we can run it to obtain a list of UTxOs.
runUtxoSearch :: (Monad m) => UtxoSearch m a -> m [(Api.TxOutRef, a)]
runUtxoSearch = ListT.toList

-- * Initial UTxO searches

-- | Search all currently known 'Api.TxOutRef's together with their corresponding
-- 'Api.TxOut'.
allUtxosSearch :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
allUtxosSearch = allUtxos >>= ListT.fromFoldable

-- | Search all 'Api.TxOutRef's at a certain address, together with their
-- 'Api.TxOut'.
utxosAtSearch :: (MonadBlockChainBalancing m, Script.ToAddress addr) => addr -> UtxoSearch m Api.TxOut
utxosAtSearch = utxosAt . Script.toAddress >=> ListT.fromFoldable

-- | Search all 'Api.TxOutRef's of a transaction, together with their
-- 'Api.TxOut'.
utxosFromCardanoTxSearch :: (Monad m) => Ledger.CardanoTx -> UtxoSearch m Api.TxOut
utxosFromCardanoTxSearch = ListT.fromFoldable . utxosFromCardanoTx

-- | Search all 'Api.TxOut's corresponding to given the list of
-- 'Api.TxOutRef's. Any 'Api.TxOutRef' that doesn't correspond to a known output
-- will be filtered out.
txOutByRefSearch :: (MonadBlockChainBalancing m) => [Api.TxOutRef] -> UtxoSearch m Api.TxOut
txOutByRefSearch orefs =
  ListT.traverse (\o -> return (o, o)) (ListT.fromFoldable orefs)
    `filterWith` txOutByRef

-- * filtering UTxO searches

-- | Transform a 'UtxoSearch' by applying a possibly partial monadic
-- transformation on each output in the stream
filterWith :: (Monad m) => UtxoSearch m a -> (a -> m (Maybe b)) -> UtxoSearch m b
filterWith (ListT as) f =
  ListT $
    as >>= \case
      Nothing -> return Nothing
      Just ((oref, a), rest) ->
        let filteredRest@(ListT bs) = filterWith rest f
         in f a >>= \case
              Nothing -> bs
              Just b -> return $ Just ((oref, b), filteredRest)

-- | Same as 'filterWith' but with a pure transformation
filterWithPure :: (Monad m) => UtxoSearch m a -> (a -> Maybe b) -> UtxoSearch m b
filterWithPure as f = filterWith as (return . f)

-- | Some as 'filterWithPure' but with a total transformation
filterWithAlways :: (Monad m) => UtxoSearch m a -> (a -> b) -> UtxoSearch m b
filterWithAlways as f = filterWithPure as (Just . f)

-- | Some as 'filterWithPure', but the transformation is taken from an optic
filterWithOptic :: (Is k An_AffineFold, Monad m) => UtxoSearch m a -> Optic' k is a b -> UtxoSearch m b
filterWithOptic as optic = filterWithPure as (^? optic)

-- | Same as 'filterWithPure' but the outputs are selected using a boolean
-- predicate, and not modified
filterWithPred :: (Monad m) => UtxoSearch m a -> (a -> Bool) -> UtxoSearch m a
filterWithPred as f = filterWithPure as $ \a -> if f a then Just a else Nothing

-- | A specific version of 'filterWithPred' where outputs must me of type
-- 'Api.TxOut' and the predicate only relies on their value
filterWithValuePred :: (Monad m) => UtxoSearch m Api.TxOut -> (Api.Value -> Bool) -> UtxoSearch m Api.Value
filterWithValuePred as = filterWithPred (filterWithAlways as Api.txOutValue)

-- | A specific version of 'filterWithValuePred' when 'Api.TxOut's are only kept
-- when they contain only ADA
filterWithOnlyAda :: (Monad m) => UtxoSearch m Api.TxOut -> UtxoSearch m Api.Value
filterWithOnlyAda as = filterWithValuePred as $ (1 ==) . length . Api.flattenValue

-- | A specific version of 'filterWithValuePred' when 'Api.TxOut's are only kept
-- when they contain non-ADA assets
filterWithNotOnlyAda :: (Monad m) => UtxoSearch m Api.TxOut -> UtxoSearch m Api.Value
filterWithNotOnlyAda as = filterWithValuePred as $ (1 <) . length . Api.flattenValue

-- * Useful composite UTxO searches with filters already applied

-- | Search for UTxOs at a specific address, which only carry address and value
-- information (no datum, staking credential, or reference script).
onlyValueOutputsAtSearch ::
  (MonadBlockChainBalancing m, Script.ToAddress addr) =>
  addr ->
  UtxoSearch m (ConcreteOutput Api.Credential () Api.Value Api.ScriptHash)
onlyValueOutputsAtSearch addr =
  utxosAtSearch addr
    `filterWithAlways` fromAbstractOutput
    `filterWithPure` isOutputWithoutDatum
    `filterWithPure` isEmptyStakingCredentialOutput
    `filterWithPred` (isNothing . view outputReferenceScriptL)

-- | Same as 'onlyValueOutputsAtSearch', but also ensures the returned outputs
-- do not contain non-ADA assets. These "vanilla" outputs are perfect candidates
-- to be used for balancing transaction and attaching collaterals.
vanillaOutputsAtSearch ::
  (MonadBlockChainBalancing m, Script.ToAddress addr) =>
  addr ->
  UtxoSearch m (ConcreteOutput Api.Credential () Api.Lovelace Api.ScriptHash)
vanillaOutputsAtSearch addr =
  onlyValueOutputsAtSearch addr
    `filterWithPure` isOnlyAdaOutput

-- | Searches for all outputs belonging to a given script
scriptOutputsSearch ::
  (MonadBlockChain m, Script.ToScriptHash s) =>
  s ->
  UtxoSearch m (ConcreteOutput s Api.OutputDatum Api.Value Api.ScriptHash)
scriptOutputsSearch s =
  allUtxosSearch
    `filterWithAlways` fromAbstractOutput
    `filterWithPure` isScriptOutputFrom s

-- | Searches for all outputs containing a given script as reference script
referenceScriptOutputsSearch ::
  (MonadBlockChain m, Script.ToScriptHash s) =>
  s ->
  UtxoSearch m (ConcreteOutput Api.Credential Api.OutputDatum Api.Value Api.ScriptHash)
referenceScriptOutputsSearch s =
  allUtxosSearch
    `filterWithAlways` fromAbstractOutput
    `filterWithPure` isReferenceScriptOutputFrom s
