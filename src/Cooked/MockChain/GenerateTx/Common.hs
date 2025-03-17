module Cooked.MockChain.GenerateTx.Common
  ( throwOnMaybe,
    throwOnString,
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

-- | Lifts a 'Nothing' as an error with an associated message.
throwOnMaybe :: (MonadError MockChainError m) => String -> Maybe a -> m a
throwOnMaybe errorMsg = maybe (throwOnString errorMsg) return

-- | Lifts a 'ToCardanoError' with an associated error message, or apply a
-- function if a value exists.
throwOnToCardanoErrorOrApply :: (MonadError MockChainError m) => String -> (a -> m b) -> Either Ledger.ToCardanoError a -> m b
throwOnToCardanoErrorOrApply errorMsg = either (throwError . MCEGenerationError . ToCardanoError errorMsg)

-- | Lifts a 'ToCardanoError' with an associated error message, or leaves the
-- value unchanged if it exists.
throwOnToCardanoError :: (MonadError MockChainError m) => String -> Either Ledger.ToCardanoError a -> m a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply return
