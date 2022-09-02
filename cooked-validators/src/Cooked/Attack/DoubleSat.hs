{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Attack.DoubleSat where

import Control.Monad
import Cooked.Attack.AddConstraints
import Cooked.Attack.Common
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Ledger as L
import qualified Ledger.Value as L
import Optics.Core
import qualified PlutusTx.Numeric as Pl

{- Note: What is a double satisfaction attack?

A double satisfaction attack consists in trying to satisfy the requirements for
what conceptually are two transactions in a single transaction, and doing so
incompletely. It succeeds whenever the requirements of two validators ovelap,
but the required outputs of the transaction are not sufficiently unique, so that
both validators see them as satisfying "their" requirement.

The mechanism is explained very well in the following analogy from the Plutus
documentation: "Suppose that two tax auditors from two different departments
come to visit you in turn to see if you’ve paid your taxes. You come up with a
clever scheme to confuse them. Your tax liability to both departments is $10, so
you make a single payment to the tax office’s bank account for $10. When the
auditors arrive, you show them your books, containing the payment to the tax
office. They both leave satisfied."

The double satisfaction attack 'doubleSatAttack' provided by this module works
by adding some extra 'Constraints' depending on some 'a's on the transaction
under modification, and on the current 'MockChainSt'ate. In particular, we can
add

- a 'SpendsScript', depending on a 'SpendsScript' on the original transaction,
- one or more 'Mints', depending on a 'SpendsScript' on the original transaction,
- a 'SpendsScript', depending on a 'PaysScript' on the original transaction,
- ...
-}
data DSSplitMode = Combine | OneOutputPerFocus

doubleSatAttack ::
  Is k A_Traversal =>
  Optic' k is TxSkel a ->
  (MockChainSt -> a -> Constraints) ->
  Wallet ->
  DSSplitMode ->
  Attack ()
doubleSatAttack optic extraConstrs attacker mode = do
  foci <- viewAttack (partsOf optic)
  mcst <- mcstAttack
  let cs = map (extraConstrs mcst) foci
  case mode of
    OneOutputPerFocus ->
      msum $
        map
          ( \c -> do
              additionalConstraints <- addConstraintsAttack c
              let addedValue = constraintBalance additionalConstraints
               in if addedValue `L.geq` mempty
                    then addPaysPKAttack (walletPKHash attacker) addedValue
                    else failingAttack
          )
          cs
    Combine -> do
      additionalConstraints <- mapM addConstraintsAttack cs
      let addedValue = foldr ((<>) . constraintBalance) mempty additionalConstraints
       in if addedValue `L.geq` mempty
            then addPaysPKAttack (walletPKHash attacker) addedValue
            else failingAttack
  addLabelAttack DoubleSatLbl
  where
    constraintBalance :: Constraints -> L.Value
    constraintBalance (is :=>: os) = inValue <> Pl.negate outValue
      where
        inValue = foldOf (traversed % valueAT) is
        outValue = foldOf (traversed % valueL) os

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
