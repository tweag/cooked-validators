module Cooked.MockChain.UtxoSearch
  ( runUtxoSearch,
    allUtxosSearch,
    allUtxosLedgerSearch,
    utxosAtSearch,
    utxosAtLedgerSearch,
    utxosFromCardanoTxSearch,
    txOutByRefSearch,
    filterWith,
    filterWithPure,
    filterWithOptic,
    filterWithPred,
    filterWithValuePred,
    filterWithOnlyAda,
    filterWithNotOnlyAda,
  )
where

import Control.Monad
import Cooked.MockChain.BlockChain
import Ledger.Tx qualified as Ledger
import ListT (ListT (..))
import ListT qualified
import Optics.Core
import Plutus.Script.Utils.Value qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl

-- * The type of UTxO searches

-- | If a UTxO is a 'TxOutRef' with some additional information, this type
-- captures a "stream" of UTxOs.
type UtxoSearch m a = ListT m (Pl.TxOutRef, a)

-- | Given a UTxO search, we can run it to obtain a list of UTxOs.
runUtxoSearch :: (Monad m) => UtxoSearch m a -> m [(Pl.TxOutRef, a)]
runUtxoSearch = ListT.toList

-- | Search all currently known 'TxOutRef's together with their corresponding
-- 'TxInfo'-'TxOut'.
allUtxosSearch :: (MonadBlockChain m) => UtxoSearch m Pl.TxOut
allUtxosSearch = allUtxos >>= ListT.fromFoldable

-- | Like 'allUtxosSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
allUtxosLedgerSearch :: (MonadBlockChain m) => UtxoSearch m Ledger.TxOut
allUtxosLedgerSearch = allUtxosLedger >>= ListT.fromFoldable

-- | Search all 'TxOutRef's at a certain address, together with their
-- 'TxInfo'-'TxOut'.
utxosAtSearch :: (MonadBlockChainBalancing m) => Pl.Address -> UtxoSearch m Pl.TxOut
utxosAtSearch = utxosAt >=> ListT.fromFoldable

-- | Like 'utxosAtSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
utxosAtLedgerSearch :: (MonadBlockChainBalancing m) => Pl.Address -> UtxoSearch m Ledger.TxOut
utxosAtLedgerSearch = utxosAtLedger >=> ListT.fromFoldable

-- | Search all 'TxOutRef's of a transaction, together with their
-- 'TxInfo'-'TxOut'.
utxosFromCardanoTxSearch :: (Monad m) => Ledger.CardanoTx -> UtxoSearch m Pl.TxOut
utxosFromCardanoTxSearch = ListT.fromFoldable . utxosFromCardanoTx

-- | Search all 'TxInfo'-'TxOut's corresponding to given the list of
-- 'TxOutRef's. Any 'TxOutRef' that doesn't correspond to a known output will be
-- filtered out.
txOutByRefSearch :: (MonadBlockChainBalancing m) => [Pl.TxOutRef] -> UtxoSearch m Pl.TxOut
txOutByRefSearch orefs =
  ListT.traverse (\o -> return (o, o)) (ListT.fromFoldable orefs)
    `filterWith` txOutByRef

-- * filtering UTxO searches

-- | Transform a 'UtxoSearch' by applying a possibly failing monadic "lookup"
-- on every output.
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

filterWithPure :: (Monad m) => UtxoSearch m a -> (a -> Maybe b) -> UtxoSearch m b
filterWithPure as f = filterWith as (return . f)

filterWithOptic :: (Is k An_AffineFold, Monad m) => UtxoSearch m a -> Optic' k is a b -> UtxoSearch m b
filterWithOptic as optic = filterWithPure as (^? optic)

filterWithPred :: (Monad m) => UtxoSearch m a -> (a -> Bool) -> UtxoSearch m a
filterWithPred as f = filterWithPure as $ \a -> if f a then Just a else Nothing

filterWithValuePred :: (Monad m) => UtxoSearch m Pl.TxOut -> (Pl.Value -> Bool) -> UtxoSearch m Pl.Value
filterWithValuePred as p = filterWithPure as $
  \txOut -> let val = Pl.txOutValue txOut in if p val then Just val else Nothing

filterWithOnlyAda :: (Monad m) => UtxoSearch m Pl.TxOut -> UtxoSearch m Pl.Value
filterWithOnlyAda as = filterWithValuePred as $ (1 ==) . length . Pl.flattenValue

filterWithNotOnlyAda :: (Monad m) => UtxoSearch m Pl.TxOut -> UtxoSearch m Pl.Value
filterWithNotOnlyAda as = filterWithValuePred as $ (1 <) . length . Pl.flattenValue
