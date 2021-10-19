module Main where

import Data.Default

import qualified Ledger          as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Ada      as Pl

import Cooked.MockChain
import Cooked.Traces
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import PMultiSig

txCreatePayment :: Wallet -> Payment -> TxSkelGen Params Params
txCreatePayment w pmt params = return (TxSkel w ctrs , params)
  where
    ctrs = [PaysScript (pmultisig params) [(Proposal pmt, paymentAmount pmt)]]




params      = Params (map walletPK knownWallets) 2
samplePmt s = Payment (Pl.lovelaceValueOf 10) (walletPKHash $ wallet 3) (Pl.slotToBeginPOSIXTime def s)

run1 = runMockChain $ do
  s <- slot
  (skel, _) <- txCreatePayment (wallet 1) (samplePmt s) params
  validateTxFromSkeleton skel
