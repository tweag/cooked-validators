module Cooked.Tx.Constraints where

import qualified Ledger.Constraints as L

import Cooked.Wallet

-- Our own constraint type; the important part is that it gets mapped
-- into L.TxConstraints
type Constraints = [L.TxConstraint]

toLedgerConstraints :: Constraints -> L.TxConstraints i o
toLedgerConstraints cs = L.TxConstraints cs [] []

-- A Transaction sekeleton is a set of our constraints, and
-- a set of our wallets, which will sign the generated transaction.
data TxSkel = TxSkel
  { txConstraints :: Constraints
  , txSigners     :: Wallet
  } deriving Show
