{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.MockChain.UtxoSearch
  ( runUtxoSearch,
    allUtxosSearch,
    allUtxosLedgerSearch,
    utxosAtSearch,
    utxosAtLedgerSearch,
    utxosFromCardanoTxSearch,
    filterWith,
    filterWithPure,
    filterWithOptic,
    filterWithPred,
  )
where

import Control.Monad
import Cooked.MockChain.BlockChain
import qualified Ledger.Tx as Ledger
import ListT (ListT (..))
import qualified ListT
import Optics.Core
import qualified Plutus.V2.Ledger.Api as Pl2

-- * The type of UTxO searches

-- | If a UTxO is a 'TxOutRef' with some additional information, this type
-- captures a "stream" of UTxOs.
type UtxoSearch m a = ListT m (Pl2.TxOutRef, a)

-- | Given a UTxO search, we can run it to obtain a list of UTxOs.
runUtxoSearch :: Monad m => UtxoSearch m a -> m [(Pl2.TxOutRef, a)]
runUtxoSearch = ListT.toList

-- | Search all currently known 'TxOutRef's together with their corresponding
-- 'TxInfo'-'TxOut'.
allUtxosSearch :: MonadBlockChain m => UtxoSearch m Pl2.TxOut
allUtxosSearch = allUtxos >>= ListT.fromFoldable

-- | Like 'allUtxosSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
allUtxosLedgerSearch :: MonadBlockChain m => UtxoSearch m Ledger.TxOut
allUtxosLedgerSearch = allUtxosLedger >>= ListT.fromFoldable

-- | Search all 'TxOutRef's at a certain address, together with their
-- 'TxInfo'-'TxOut'.
utxosAtSearch :: MonadBlockChainBalancing m => Pl2.Address -> UtxoSearch m Pl2.TxOut
utxosAtSearch = utxosAt >=> ListT.fromFoldable

-- | Like 'utxosAtSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
utxosAtLedgerSearch :: MonadBlockChainBalancing m => Pl2.Address -> UtxoSearch m Ledger.TxOut
utxosAtLedgerSearch = utxosAtLedger >=> ListT.fromFoldable

-- | Search all 'TxOutRef's of a transaction, together with their
-- 'TxInfo'-'TxOut'.
utxosFromCardanoTxSearch :: Monad m => Ledger.CardanoTx -> UtxoSearch m Pl2.TxOut
utxosFromCardanoTxSearch = ListT.fromFoldable . utxosFromCardanoTx

-- * filtering UTxO searches

-- | Transform a 'UtxoSearch' by applying a possibly failing monadic "lookup"
-- on every output.
filterWith :: Monad m => UtxoSearch m a -> (a -> m (Maybe b)) -> UtxoSearch m b
filterWith (ListT as) f =
  ListT $
    as >>= \case
      Nothing -> return Nothing
      Just ((oref, a), rest) ->
        let filteredRest@(ListT bs) = filterWith rest f
         in f a >>= \case
              Nothing -> bs
              Just b -> return $ Just ((oref, b), filteredRest)

filterWithPure :: Monad m => UtxoSearch m a -> (a -> Maybe b) -> UtxoSearch m b
filterWithPure as f = filterWith as (return . f)

filterWithOptic :: (Is k An_AffineFold, Monad m) => UtxoSearch m a -> Optic' k is a b -> UtxoSearch m b
filterWithOptic as optic = filterWithPure as (^? optic)

filterWithPred :: Monad m => UtxoSearch m a -> (a -> Bool) -> UtxoSearch m a
filterWithPred as f = filterWithPure as $ \a -> if f a then Just a else Nothing
