{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Generator where

import Data.Void
import Control.Monad.Except
import Ledger.Typed.Scripts.Validators
import Ledger.Constraints
import Ledger.Tx
import Ledger.Crypto
import Ledger.Orphans                   ()
import Plutus.Contract.Trace
import qualified Plutus.V1.Ledger.Api             as Api
import qualified Plutus.V1.Ledger.Ada             as Ada

import Cooked.MockChain
import Cooked.Balance
import Cooked.Tx.Constraints

-- * MockChain Example
--
-- Start from the initial 'UtxoIndex' and transfer 4200 lovelace from wallet 1 to wallet 2

example :: IO (Either MockChainError ())
example = runMockChainIO mcState0 $ do
  validateTxFromConstraints @Void (mcWallet 1) mempty (mustPayToPubKey (mcWalletPKHash $ mcWallet 2) (Ada.lovelaceValueOf 4200))

-- |Generates a transaction from constraints and signs it as if it were from the given Wallet.
validateTxFromConstraints :: forall a m
                           . ( Monad m, Api.FromData (DatumType a)
                             , Api.FromData (RedeemerType a), Api.ToData (DatumType a), Api.ToData (RedeemerType a))
                          => (Wallet, PrivateKey)
                          -> ScriptLookups a
                          -> TxConstraints (RedeemerType a) (DatumType a)
                          -> MockChainT m ()
validateTxFromConstraints (w, sk) lkups constr = do
  let etx = mkTx lkups constr
  case etx of
    Left err -> throwError (MCETxError err)
    Right tx -> balanceTxFrom w tx >>= validateTx . addSignature sk

validateTxFromConstraints' :: forall a m
                            . (Monad m, Api.FromData (DatumType a), Api.FromData (RedeemerType a), Api.ToData (DatumType a), Api.ToData (RedeemerType a))
                           => (Wallet, PrivateKey)
                           -> [(ScriptLookups a, TxConstraints (RedeemerType a) (DatumType a))]
                           -> MockChainT m ()
validateTxFromConstraints' wsk cstr =
  validateTxFromConstraints wsk (mconcat $ map fst cstr) (mconcat $ map snd cstr)

validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton (TxSkel constr ws) =
  validateTxFromConstraints @Void ws mempty (toLedgerConstraints constr)
