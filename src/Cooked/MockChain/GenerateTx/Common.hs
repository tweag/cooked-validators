-- | Common utilities used to transfer generation errors raised by plutus-ledger
-- into instances of 'MockChainError'
module Cooked.MockChain.GenerateTx.Common
  ( throwOnString,
    throwOnToCardanoErrorOrApply,
    throwOnToCardanoError,
  )
where

import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Ledger.Tx qualified as Ledger

-- | Throws a general error from a String.
throwOnString :: (MonadError MockChainError m) => String -> m a
throwOnString = throwError . MCEGenerationError . GenerateTxErrorGeneral

-- | Lifts a 'ToCardanoError' with an associated error message, or apply a
-- function if a value exists.
throwOnToCardanoErrorOrApply :: (MonadError MockChainError m) => String -> (a -> b) -> Either Ledger.ToCardanoError a -> m b
throwOnToCardanoErrorOrApply errorMsg f = either (throwError . MCEGenerationError . ToCardanoError errorMsg) (return . f)

-- | Lifts a 'ToCardanoError' with an associated error message, or leaves the
-- value unchanged if it exists.
throwOnToCardanoError :: (MonadError MockChainError m) => String -> Either Ledger.ToCardanoError a -> m a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply id
