{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Generator where

import Control.Monad.Except
import Cooked.MockChain
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import qualified Ledger.Tx as Pl

-- | Generates a balanced transaction, that is, a transaction where
--  @inputs + mints == outputs + fees@. To balance a transaction, we need
--  access to the current UTxO state to chose which inputs to add in case
--  the output-side of the balancing equation is bigger.
generateTx :: (Monad m) => TxSkel -> MockChainT m Pl.Tx
generateTx skel =
  case generateUnbalTx skel of
    Left err -> throwError $ MCETxError err
    Right ubtx -> txAddSignature (txSigners skel) <$> balanceTxFrom (txSigners skel) ubtx

-- | Generates, balances and validates a transaction from a 'TxSkel'
validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton = generateTx >=> validateTx
