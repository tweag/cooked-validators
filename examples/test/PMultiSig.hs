module Main where

import Data.Default

import qualified Ledger           as Pl
import qualified Ledger.TimeSlot  as Pl
import qualified Ledger.Ada       as Pl
import qualified PlutusTx.Prelude as Pl

import Cooked.MockChain
import Cooked.Traces
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import PMultiSig

params :: Params
params = Params (map walletPK knownWallets) 2

txCreatePayment :: Wallet -> Payment -> MockChain TxSkel
txCreatePayment w pmt = return (TxSkel w ctrs)
  where
    ctrs = [PaysScript (pmultisig params) [(Proposal pmt, Pl.lovelaceValueOf (paymentAmount pmt))]]

txSign :: Wallet -> Payment -> MockChain TxSkel
txSign w pmt =
  let wSignature = Pl.sign (Pl.sha2_256 $ packPayment pmt) (snd w)
      wPk        = walletPK w
      ctrs       = [PaysScript (pmultisig params) [(Sign wPk wSignature, mempty)]]
   in return (TxSkel w ctrs)

isProposal :: Datum -> a -> Bool
isProposal (Proposal _) _ = True
isProposal _            _ = False

isSignature :: Datum -> a -> Bool
isSignature (Sign _ _) _ = True
isSignature _          _ = False

txExecute :: Wallet -> MockChain TxSkel
txExecute w = do
  [(propOut, Proposal prop)] <- scriptUtxosSuchThat (pmultisig params) isProposal
  sigs              <- scriptUtxosSuchThat (pmultisig params) isSignature
  let txConstr = PaysPK (paymentRecipient prop) (Pl.lovelaceValueOf $ paymentAmount prop)
               : SpendsScript (pmultisig params) () (propOut, Proposal prop)
               : map (SpendsScript (pmultisig params) ()) sigs
  return (TxSkel w txConstr)

samplePmt = Payment 4200 (walletPKHash $ wallet 3)

run1 = runMockChain $ do
  txCreatePayment (wallet 1) samplePmt >>= validateTxFromSkeleton
  txSign          (wallet 1) samplePmt >>= validateTxFromSkeleton
  txSign          (wallet 2) samplePmt >>= validateTxFromSkeleton
  txSign          (wallet 3) samplePmt >>= validateTxFromSkeleton
  txExecute       (wallet 4) >>= validateTxFromSkeleton
