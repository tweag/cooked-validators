{-# LANGUAGE FlexibleContexts #-}

module Cooked.Attack.DupToken where

import Control.Monad
import Cooked.Attack.Common
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Ledger as L
import Optics.Core
import qualified PlutusTx.Numeric as Pl

-- | Change some values in a 'TxSkel'. Returns a list of the increments by which
-- all of the focused values were changed.
changeValueAttack ::
  Is k A_Traversal =>
  Optic' k is TxSkel L.Value ->
  -- | A function describing how a focused value should change. Return the
  -- modified value.
  (L.Value -> L.Value) ->
  Attack [L.Value]
changeValueAttack optic change =
  mkAccumLAttack
    optic
    ( \_mcst acc oldValue ->
        let newValue = change oldValue
         in (newValue, acc ++ [newValue <> Pl.negate oldValue])
    )
    []

-- | Add a 'paysPK' constraint to a transaction. The additional constraint will
-- be added to the end of the list of output constraints.
addPaysPKAttack ::
  L.PubKeyHash ->
  L.Value ->
  Attack ()
addPaysPKAttack h v = Attack $ \_mcst skel ->
  [(over outConstraintsL (++ [paysPK h v]) skel, ())]

-- | A token duplication attack increases values in 'Mints'-constraints of a
-- 'TxSkel' according to some conditions, and pays the extra minted value to a
-- given recipient wallet. This adds a 'DupTokenLbl' to the labels of the
-- transaction using 'addLabel'. Returns the 'Value' by which the minted value
-- was increased.
dupTokenAttack ::
  -- | A function describing how the amount of minted tokens of an asset class
  -- should be changed. The given function @f@ should satisfy @f ac i > i@ for
  -- all @ac@ and @i@, i.e. it should increase the minted amount. If it does
  -- *not* increase the minted amount, it will be left unchanged.
  (L.AssetClass -> Integer -> Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  Attack L.Value
dupTokenAttack change attacker = do
  increments <- changeValueAttack (mintsConstraintsT % valueL) increaseValue
  guard (any (/= mempty) increments)
  let totalIncrement = mconcat increments
  addPaysPKAttack (walletPKHash attacker) totalIncrement
  addLabelAttack DupTokenLbl
  return totalIncrement
  where
    increaseValue :: L.Value -> L.Value
    increaseValue =
      over
        flattenValueI
        ( map
            ( \(ac, i) ->
                let j = change ac i
                 in if j > i
                      then (ac, j)
                      else (ac, i)
            )
        )

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show)
