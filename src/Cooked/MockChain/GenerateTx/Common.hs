module Cooked.MockChain.GenerateTx.Common
  ( GenerateTxError (..),
    TxGen,
    Transform (..),
    throwOnLookup,
    throwOnString,
    throwOnToCardanoErrorOrApply,
    throwOnToCardanoError,
    liftTxGen,
  )
where

import Cardano.Api.Shelley qualified as Cardano
import Control.Monad.Reader
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Tx qualified as Ledger

-- * Domain for transaction generation and associated types

-- | Errors that can arise during transaction generation
data GenerateTxError
  = ToCardanoError String Ledger.ToCardanoError
  | TxBodyError String Cardano.TxBodyError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

-- | The domain in which transaction parts are generated.
type TxGen context a = ReaderT context (Either GenerateTxError) a

class Transform a b where
  transform :: a -> b

instance Transform (a, b) b where
  transform = snd

instance Transform (a, b) a where
  transform = fst

-- | Lifts a computation from a smaller context
liftTxGen :: (Transform context' context) => TxGen context a -> TxGen context' a
liftTxGen comp = (lift . runReaderT comp) =<< asks transform

-- | Looks up a key in a map. Throws a 'GenerateTxErrorGeneral' error with a given
-- message when the key is absent, returns the associated value otherwise.
throwOnLookup :: (Ord k) => String -> k -> Map k a -> TxGen context a
throwOnLookup errorMsg key = maybe (throwOnString errorMsg) return . Map.lookup key

-- | Throws a general error from a String.
throwOnString :: String -> TxGen context a
throwOnString = lift . Left . GenerateTxErrorGeneral

-- | Lifts a 'ToCardanoError' with an associated error message, or apply a
-- function if a value exists.
throwOnToCardanoErrorOrApply :: String -> (a -> b) -> Either Ledger.ToCardanoError a -> TxGen context b
throwOnToCardanoErrorOrApply errorMsg f = lift . bimap (ToCardanoError errorMsg) f

-- | Lifts a 'ToCardanoError' with an associated error message, or leaves the
-- value unchanged if it exists.
throwOnToCardanoError :: String -> Either Ledger.ToCardanoError a -> TxGen context a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply id
