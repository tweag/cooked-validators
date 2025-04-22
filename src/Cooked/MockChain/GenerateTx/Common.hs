-- | Common utilities used to transfer generation errors raised by plutus-ledger
-- into instances of 'MockChainError'
module Cooked.MockChain.GenerateTx.Common
  ( throwOnToCardanoErrorOrApply,
    throwOnToCardanoError,
  )
where

import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Ledger.Tx qualified as Ledger

-- | Lifts a 'Ledger.ToCardanoError' with an associated error message, or apply a
-- function if a value exists.
throwOnToCardanoErrorOrApply :: (MonadError MockChainError m) => String -> (a -> b) -> Either Ledger.ToCardanoError a -> m b
throwOnToCardanoErrorOrApply errorMsg f = either (throwError . MCEToCardanoError errorMsg) (return . f)

-- | Lifts a 'Ledger.ToCardanoError' with an associated error message, or leaves
-- the value unchanged if it exists.
throwOnToCardanoError :: (MonadError MockChainError m) => String -> Either Ledger.ToCardanoError a -> m a
throwOnToCardanoError = flip throwOnToCardanoErrorOrApply id
