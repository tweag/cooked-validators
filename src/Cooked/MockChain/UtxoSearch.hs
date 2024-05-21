-- | This module provides a convenient framework to look through UTxOs and
-- search relevant ones based on predicates. For instance, it makes it very
-- convenient to gather all UTxOs at a certain address.
module Cooked.MockChain.UtxoSearch where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Cooked.Classes
import Cooked.MockChain.BlockChain
import Cooked.Output
import Data.Maybe
import Ledger.Tx qualified as Ledger
import ListT (ListT (..))
import ListT qualified
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-----------------------------------

-- * The type of UTxO searches * --

-----------------------------------

-- | If a UTxO is a 'TxOutRef' with some additional information, this type
-- captures a "stream" of potential UTxOs.
type UtxoSearch m a = MaybeT (ListT m) (Api.TxOutRef, a)

-- | Given a UTxO search, we can run it to obtain a list of actual UTxOs.
runUtxoSearch :: (Monad m) => UtxoSearch m a -> m [(Api.TxOutRef, a)]
runUtxoSearch = (catMaybes <$>) . ListT.toList . runMaybeT

------------------------------------

-- * Filters to trim down utxos * --

------------------------------------

-- | A filter changes `a` into a potential `b` within a certain
-- context m. These filters are more powerful than the usual filters
-- as they are type-changing. Thus they will only be composable in a
-- certain type-compatible fashion
type Filter m a b = a -> m (Maybe b)

-- | Creates a filter from a filtering function
idFilter :: (a -> m (Maybe b)) -> Filter m a b
idFilter = id

-- | Creates a filter from a pure filtering function
pureFilter :: (Monad m) => (a -> Maybe b) -> Filter m a b
pureFilter = (return .)

-- | Creates a filter from a pure always succeeding filter
pureAlwaysFilter :: (Monad m) => (a -> b) -> Filter m a b
pureAlwaysFilter = pureFilter . (Just .)

-- | Creates a filter from a monadic always succeeding filter
alwaysFilter :: (Monad m) => (a -> m b) -> Filter m a b
alwaysFilter = ((Just <$>) .)

-- | Creates a filter from a pure selection function
pureBoolFilter :: (Monad m) => (a -> Bool) -> Filter m a a
pureBoolFilter af = pureFilter (\v -> if af v then Just v else Nothing)

-- | Creates a filter from a monadic selection function
boolFilter :: (Monad m) => (a -> m Bool) -> Filter m a a
boolFilter af v = (\x -> if x then Just v else Nothing) <$> af v

-- | Creates a filter from an optic
opticFilter :: (Monad m, Is k An_AffineFold) => Optic' k is a b -> Filter m a b
opticFilter = pureFilter . (\o -> (^? o))

-- | Negates a filter, that is keep elements which should be filtered
-- out. Does not produce any casting.
negateF :: (Monad m) => Filter m a b -> Filter m a a
negateF f a = maybe (Just a) (const Nothing) <$> f a

---------------------------------------

-- * Searching and filtering utxos * --

---------------------------------------

-- | Refines a search using a filter
(*+*) :: (Monad m) => UtxoSearch m a -> Filter m a b -> UtxoSearch m b
(*+*) s f = s >>= \(txOutRef, output) -> (txOutRef,) <$> MaybeT (lift $ f output)

-- | Refines a seach using a negated filter
(*~*) :: (Monad m) => UtxoSearch m a -> Filter m a b -> UtxoSearch m a
(*~*) s = (s *+*) . negateF

--------------------------

-- * Initial searches * --

--------------------------

-- | Search all 'TxOutRef's at a certain address, together with their
-- 'TxInfo'-'TxOut'.
utxosAtSearch :: (MonadBlockChainBalancing m, HasAddress addr) => addr -> UtxoSearch m Api.TxOut
utxosAtSearch = lift . (utxosAt >=> ListT.fromFoldable) . toAddress

-- | Like 'utxosAtSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
utxosAtLedgerSearch :: (MonadBlockChainBalancing m, HasAddress addr) => addr -> UtxoSearch m Ledger.TxOut
utxosAtLedgerSearch = lift . (utxosAtLedger >=> ListT.fromFoldable) . toAddress

-- | Search all currently known 'TxOutRef's together with their corresponding
-- 'TxInfo'-'TxOut'.
allUtxosSearch :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
allUtxosSearch = lift $ allUtxos >>= ListT.fromFoldable

-- | Like 'allUtxosSearch', but returns a Ledger-level representation of the
-- transaction outputs, which might contain more information.
allUtxosLedgerSearch :: (MonadBlockChain m) => UtxoSearch m Ledger.TxOut
allUtxosLedgerSearch = lift $ allUtxosLedger >>= ListT.fromFoldable

-- | Initializes a search with all known Concrete outputs
concreteOutputSearch :: (MonadBlockChain m) => UtxoSearch m (ConcreteOutput Api.Credential Api.OutputDatum Api.Value Api.ScriptHash)
concreteOutputSearch = allUtxosSearch *+* pureAlwaysFilter fromAbstractOutput

-- | Initializes a search with utxos from a specific transaction
utxosFromCardanoTxSearch :: (Monad m) => Ledger.CardanoTx -> UtxoSearch m Api.TxOut
utxosFromCardanoTxSearch = lift . ListT.fromFoldable . utxosFromCardanoTx

-- | Initializes a search with resolved utxos from a given list
txOutByRefSearch :: (MonadBlockChainBalancing m) => [Api.TxOutRef] -> UtxoSearch m Api.TxOut
txOutByRefSearch orefs = do
  txOutRef <- lift $ ListT.fromFoldable orefs
  txOut <- MaybeT $ txOutByRef txOutRef
  return (txOutRef, txOut)

-----------------------------------------
-- Specific cases of filtered searches --
-----------------------------------------

onlyAdaSearch :: (MonadBlockChain m) => UtxoSearch m (ConcreteOutput Api.Credential Api.OutputDatum Script.Ada Api.ScriptHash)
onlyAdaSearch =
  concreteOutputSearch
    *+* pureFilter isOnlyAdaOutput

notAdaOnlySearch :: (MonadBlockChain m) => UtxoSearch m (ConcreteOutput Api.Credential Api.OutputDatum Api.Value Api.ScriptHash)
notAdaOnlySearch =
  concreteOutputSearch
    *~* pureFilter isOnlyAdaOutput

vanillaUtxosSearch :: (MonadBlockChain m) => UtxoSearch m (ConcreteOutput Api.Credential () Script.Ada Api.ScriptHash)
vanillaUtxosSearch =
  onlyAdaSearch
    *+* pureFilter isOutputWithoutDatum
    *+* pureBoolFilter (isNothing . view outputReferenceScriptL)

vanillaUtxosAtSearch :: (MonadBlockChainBalancing m, HasAddress addr) => addr -> UtxoSearch m (ConcreteOutput Api.Credential () Script.Ada Api.ScriptHash)
vanillaUtxosAtSearch addr =
  utxosAtSearch addr
    *+* pureAlwaysFilter fromAbstractOutput
    *+* pureFilter isOnlyAdaOutput
    *+* pureFilter isOutputWithoutDatum

scriptOutputSearch :: (MonadBlockChain m) => Script.TypedValidator a -> UtxoSearch m (ConcreteOutput (Script.TypedValidator a) Api.OutputDatum Api.Value Api.ScriptHash)
scriptOutputSearch tv =
  concreteOutputSearch
    *+* pureFilter (isScriptOutputFrom tv)
