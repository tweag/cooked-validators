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

-- | How to combine extra constraints added for different reasons in the
-- 'doubleSatAttack'. See the comments there.
data DSSplitMode = TryCombinations | AllSeparate

-- | Double satisfacion attack. See the comment above for what such an attack is
-- about conceptually.
--
-- This attack consists in adding some extra constraints to a transaction, and
-- hoping that the additional checks triggered by the extra inputs are fooled by
-- what's already present on the transaction. Any extra value contained in the
-- added constraints is then paid to the attacker.
doubleSatAttack ::
  Is k A_Traversal =>
  -- | Each focus of this optic is a potential reason to add some extra
  -- constraints.
  --
  -- As an example, one could go through the 'PaysScript' constraints for
  -- validators of type @t@ with the following traversal:
  --
  -- > paysScriptConstraintsT % paysScriptConstraintTypeP @t
  Optic' k is TxSkel a ->
  -- | Which constraints to add, for each of the foci. There might be different
  -- options for each focus, that's why the return value is a list.
  --
  -- Coninuing the example, for each of the focused 'PaysScript' constraints,
  -- you might want to try adding some 'SpendsScript' constraints to the
  -- transaction. Since it might be interesting to try different redeemers on
  -- these extra 'SpendsScript' constraints, you can just provide a list of all
  -- the options you want to try adding for a given 'PaysScript' that's already
  -- on the transaction.
  (MockChainSt -> a -> [Constraints]) ->
  -- | The wallet of the attacker, where any surplus is paid to.
  --
  -- In the example, the extra value in the added 'SpendsScript' constraints
  -- will be paid to the attacker.
  Wallet ->
  -- | Since there are potentially many options for each of the foci, the
  -- question is whether (and how) to combine additions that were triggered by
  -- different foci.
  --
  -- In the example: Let's say that the unmodified transaction had three focused
  -- 'PaysScript' constraints, and that you want to try 2, 3, and 5 options for
  -- additional 'SpendsScript' constraints for each of them, respectively.
  --
  -- - If you want to try each additional 'SpendsScript' on its own modified
  --   transaction, use 'AllSeparate'. Thus, there'll be 2+3+5=10 modified
  --   transactions.
  --
  -- - If you want to try combining all options from one focus with all options
  --   from all of the other foci, use 'TryCombinations'. Then, there'll be
  --   (2+1)*(3+1)*(5+1)=72 possible combinations, if you include all of the
  --   combinations where /at most/ three (one for each focus) extra constraints
  --   are added. One of these combinations is of course the one where nothing
  --   is added, and that one is omitted, which brings the grand total down to
  --   71 options.
  --
  -- So you see that this attack can branch quite wildly. Use with caution!
  DSSplitMode ->
  Attack ()
doubleSatAttack optic extra attacker mode = do
  mcst <- mcstAttack
  extraConstrs <- map (extra mcst) <$> viewAttack (partsOf optic)
  case mode of
    AllSeparate ->
      msum $
        map
          ( \c -> do
              added <- addConstraintsAttack c
              let addedValue = constraintBalance added
              if addedValue `L.geq` mempty
                then addPaysPKAttack (walletPKHash attacker) addedValue
                else failingAttack
          )
          (concat extraConstrs)
    TryCombinations ->
      msum $
        map
          ( \cs -> do
              added <- mapM addConstraintsAttack cs
              let addedValue = foldr ((<>) . constraintBalance) mempty added
              if addedValue `L.geq` mempty
                then addPaysPKAttack (walletPKHash attacker) addedValue
                else failingAttack
          )
          (tail $ allCombinations $ map (mempty :) extraConstrs)
  addLabelAttack DoubleSatLbl
  where
    constraintBalance :: Constraints -> L.Value
    constraintBalance (is :=>: os) = inValue <> Pl.negate outValue
      where
        inValue = foldOf (traversed % valueAT) is
        outValue = foldOf (traversed % valueL) os

    allCombinations :: [[x]] -> [[x]]
    allCombinations (l : ls) = let cs = allCombinations ls in concatMap (\x -> (x :) <$> cs) l
    allCombinations [] = [[]]

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
