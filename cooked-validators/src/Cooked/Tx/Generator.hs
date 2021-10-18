{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cooked.Tx.Generator where

import Data.Void
import Control.Monad.Except
import Control.Monad.State

import qualified Ledger.Typed.Scripts.Validators as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Index as Pl
import qualified Ledger.Crypto as Pl
import qualified Plutus.Contract.Trace as Pl
import qualified Plutus.V1.Ledger.Api as Pl

import Cooked.MockChain
import Cooked.Tx.Balance
import Cooked.Tx.Constraints


-- |Validates a transaction and, upon success, updates the utxo map; Since constructing
-- transactions can be painful, you probably want to use 'validateTxFromConstraints'
-- instead.
validateTx :: (Monad m) => Pl.Tx -> MockChainT m ()
validateTx tx = do
  s  <- slot
  ix <- gets mcstIndex
  let res = Pl.runValidation (Pl.validateTransaction s tx) ix
  -- uncomment to see the ScriptValidationEvents; could be useful for debugging but it
  -- gets a bit noisy.
  --
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _)  -> throwError (MCEValidationError err)
    (Nothing, ix') -> modify (\st -> st { mcstIndex = ix' })


-- |Generates a transaction from constraints and signs it as if it were from the given Wallet.
validateTxFromConstraints :: forall a m
                           . ( Monad m, Pl.FromData (Pl.DatumType a)
                             , Pl.FromData (Pl.RedeemerType a), Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
                          => Wallet
                          -> Pl.ScriptLookups a
                          -> Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a)
                          -> MockChainT m ()
validateTxFromConstraints w lkups constr = do
  let etx = Pl.mkTx lkups constr
  case etx of
    Left err -> throwError (MCETxError err)
    Right tx -> balanceTxFrom w tx >>= validateTx . Pl.addSignature (snd w)

validateTxFromConstraints' :: forall a m
                            . (Monad m, Pl.FromData (Pl.DatumType a), Pl.FromData (Pl.RedeemerType a), Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
                           => (Pl.Wallet, Pl.PrivateKey)
                           -> [(Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))]
                           -> MockChainT m ()
validateTxFromConstraints' wsk cstr =
  validateTxFromConstraints wsk (mconcat $ map fst cstr) (mconcat $ map snd cstr)

validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton (TxSkel ws constr) =
  uncurry (validateTxFromConstraints @Void ws) (toLedgerConstraints constr)
