{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cooked.Tx.Generator where

import Control.Monad.Except
import Control.Monad.State (modify)

import qualified Ledger.Tx as Pl

import Cooked.MockChain
import Cooked.Tx.Balance
import Cooked.Tx.Constraints

import qualified Data.Map as Map (unions)

-- |Generates a balanced transaction, that is, a transaction where
-- @inputs + mints == outputs + fees@. To balance a transaction, we need
-- access to the current UTxO state to chose which inputs to add in case
-- the output-side of the balancing equation is bigger.
generateTx :: (Monad m) => TxSkel -> MockChainT m Pl.Tx
generateTx skel = do
  modify $ updateDatumStr skel
  case generateUnbalTx skel of
    Left err   -> throwError $ MCETxError err
    Right ubtx -> txAddSignature (txSigners skel) <$> balanceTxFrom (txSigners skel) ubtx
  where
    -- Update the map of pretty printed representations in the mock chain state
    updateDatumStr :: TxSkel -> MockChainSt -> MockChainSt
    updateDatumStr TxSkel {txConstraints} st@MockChainSt {mcstStrDatums} =
      st { mcstStrDatums =
        Map.unions $
        mcstStrDatums : (extractDatumStrFromConstraint <$> txConstraints)
      }

-- |Generates, balances and validates a transaction from a 'TxSkel'
validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton = generateTx >=> validateTx
