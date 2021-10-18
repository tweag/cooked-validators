{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Generator where

import Data.Void
import Control.Monad.Except

import qualified Ledger.Typed.Scripts.Validators as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Crypto as Pl
import qualified Plutus.Contract.Trace as Pl
import qualified Plutus.V1.Ledger.Api as Pl
import qualified Plutus.V1.Ledger.Ada as Pl

import Cooked.MockChain
import Cooked.Balance
import Cooked.Tx.Constraints

-- * MockChain Example
--
-- Start from the initial 'UtxoIndex' and transfer 4200 lovelace from wallet 1 to wallet 2

example :: IO (Either MockChainError ())
example = runMockChainIO mcState0 $ do
  validateTxFromConstraints @Void (mcWallet 1) mempty (Pl.mustPayToPubKey (mcWalletPKHash $ mcWallet 2) (Pl.lovelaceValueOf 4200))

-- |Generates a transaction from constraints and signs it as if it were from the given Wallet.
validateTxFromConstraints :: forall a m
                           . ( Monad m, Pl.FromData (Pl.DatumType a)
                             , Pl.FromData (Pl.RedeemerType a), Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
                          => (Pl.Wallet, Pl.PrivateKey)
                          -> Pl.ScriptLookups a
                          -> Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a)
                          -> MockChainT m ()
validateTxFromConstraints (w, sk) lkups constr = do
  let etx = Pl.mkTx lkups constr
  case etx of
    Left err -> throwError (MCETxError err)
    Right tx -> balanceTxFrom w tx >>= validateTx . Pl.addSignature sk

validateTxFromConstraints' :: forall a m
                            . (Monad m, Pl.FromData (Pl.DatumType a), Pl.FromData (Pl.RedeemerType a), Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
                           => (Pl.Wallet, Pl.PrivateKey)
                           -> [(Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))]
                           -> MockChainT m ()
validateTxFromConstraints' wsk cstr =
  validateTxFromConstraints wsk (mconcat $ map fst cstr) (mconcat $ map snd cstr)

validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton (TxSkel constr ws) =
  validateTxFromConstraints @Void ws mempty (toLedgerConstraints constr)
