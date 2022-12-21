{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.Attack.DoubleSat where

import Control.Monad
import Cooked.Attack.Tweak
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
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
by going through the foci of some optic on the 'TxSkel' representing the
transaction from the left to the right, and adding some extra inputs, outputs,
and mints depending on each focus and the current 'MockChainSt'ate. -}

-- | How to combine extra constraints added for different reasons in the
-- 'doubleSatAttack'. See the comments there.
data DoubleSatSplitMode = TryCombinations | AllSeparate

-- | A triple of transaction inputs, transaction outputs, and minted value. This
-- is what we can add to the transaction in order to try a double satisfaction
-- attack.
type DoubleSatDelta = (Map SpendableOut TxSkelIn, [TxSkelOut], TxSkelMints)

instance {-# OVERLAPPING #-} Semigroup DoubleSatDelta where
  (i, o, m) <> (i', o', m') =
    ( i <> i', -- this is left-biased union
      o ++ o',
      m <> m' -- see the 'Semigroup' instance of 'TxSkelMints'
    )

instance {-# OVERLAPPING #-} Monoid DoubleSatDelta where
  mempty = (Map.empty, [], mempty)

-- | Double satisfacion attack. See the comment above for what such an attack is
-- about conceptually.
--
-- This attack consists in adding some extra constraints to a transaction, and
-- hoping that the additional minting policies or validator scripts thereby
-- involved are fooled by what's already present on the transaction. Any extra
-- value contained in new inputs to the transaction is then paid to the
-- attacker.
doubleSatAttack ::
  Is k A_Fold =>
  -- | Each focus of this optic is a potential reason to add some extra
  -- constraints.
  --
  -- As an example, one could go through the 'PaysScript' outputs for
  -- validators of type @t@ with the following traversal:
  --
  -- > paysScriptTypeT @t
  Optic' k is TxSkel a ->
  -- | Which inputs, outputs, and mints to add, for each of the foci. There
  -- might be different options for each focus, that's why the return value is a
  -- list.
  --
  -- Continuing the example, for each of the focused 'PaysScript' outputs, you
  -- might want to try adding some 'SpendsScript' inputs to the
  -- transaction. Since it might be interesting to try different redeemers on
  -- these extra 'SpendsScript' inputs, you can just provide a list of all the
  -- options you want to try adding for a given 'PaysScript' that's already on
  -- the transaction.
  (MockChainSt -> a -> [DoubleSatDelta]) ->
  -- | The wallet of the attacker, where any surplus is paid to.
  --
  -- In the example, the extra value in the added 'SpendsScript' constraints
  -- will be paid to the attacker.
  Wallet ->
  -- | Since there are potentially multiple triples of inputs, outputs, and
  -- mints produced for each of the foci, the question is whether (and how) to
  -- combine additions that were triggered by different foci.
  --
  -- In the example: Let's say that the unmodified transaction had three focused
  -- 'PaysScript' outputs (let's denote them by a, b, and c), and that you want
  -- to try 2, 3, and 5 additional 'SpendsScript' inputs for each of them,
  -- respectively (let's call those a1, a2, and b1, b2, b3, and c1, c2, c3, c4,
  -- c5).
  --
  -- - If you want to try each additional 'SpendsScript' on its own modified
  --   transaction, use 'AllSeparate'. Thus, there'll be 2+3+5=10 modified
  --   transactions. Namely, for each element of the list
  --
  -- > [a1, a2, b1, b2, b3, c1, c2, c3, c4, c5]  ,
  --
  --   you'll get one modified transaction that includes that additional input.
  --
  -- - If you want to try combining all options from one focus with all options
  --   from all of the other foci, use 'TryCombinations'. Then, there'll be
  --   (2+1)*(3+1)*(5+1)=72 possible combinations, if you include all of the
  --   combinations where /at most/ three (one for each focus) extra constraints
  --   are added. One of these combinations is of course the one where nothing
  --   is added, and that one is omitted, which brings the grand total down to
  --   71 modified transactions, the additional 'SpendsScript' inputs of which
  --   are given by the following list:
  --
  -- > [ -- one additional input (the 10 cases from above)
  -- >   [a1],
  -- >   [a2],
  -- >   ...
  -- >   [c4],
  -- >   [c5],
  -- >
  -- >   -- two additional inputs, from different foci (2*3 + 2*5 + 3*5 = 31 cases)
  -- >   [a1, b1],
  -- >   [a1, b2],
  -- >   ...
  -- >   [b3, c4],
  -- >   [b3, c5],
  -- >
  -- >   -- three additional inputs, one from each focus (2*3*5 = 30 cases)
  -- >   [a1, b1, c1],
  -- >   [a1, b1, c2],
  -- >   ...
  -- >   [a1, b3, c4],
  -- >   [a1, b3, c5]
  -- > ]
  --
  -- So you see that this attack can branch quite wildly. Use with caution!
  DoubleSatSplitMode ->
  Tweak ()
doubleSatAttack optic extra attacker mode = do
  mcst <- mcstTweak
  deltas <- map (extra mcst) <$> viewAllTweak optic
  msum $
    map
      ( \delta -> do
          addDoubleSatDeltaTweak delta
          let addedValue = deltaBalance delta
          if addedValue `L.gt` mempty
            then addOutputTweak $ paysPK (walletPKHash attacker) addedValue
            else failingTweak
      )
      ( case mode of
          AllSeparate -> nubBy sameDeltas $ concat deltas
          TryCombinations ->
            nubBy sameDeltas $
              map joinDeltas $
                tail $ allCombinations $ map (mempty :) deltas
      )
  addLabelTweak DoubleSatLbl
  where
    -- for each triple of additional inputs, outputs, and mints, calculate its balance
    deltaBalance :: DoubleSatDelta -> L.Value
    deltaBalance (inputs, outputs, mints) = inValue <> Pl.negate outValue <> mintValue
      where
        inValue = foldOf (traversed % sOutValueL) $ Map.keys inputs
        outValue = foldOf (traversed % outValueL) outputs
        mintValue = txSkelMintsValue mints

    -- Helper tweak to add a 'DoubleSatDelta' to a transaction
    addDoubleSatDeltaTweak :: DoubleSatDelta -> Tweak ()
    addDoubleSatDeltaTweak (ins, outs, mints) =
      mapM_ (uncurry addInputTweak) (Map.toList ins)
        >> mapM_ addOutputTweak outs
        >> mapM_ addMintTweak (txSkelMintsToList mints)

    -- Join a list of 'DoubleSatDelta's into one 'DoubleSatDelta' that specifies
    -- eveything that is contained in the input.
    joinDeltas :: [DoubleSatDelta] -> DoubleSatDelta
    joinDeltas = mconcat

    sameDeltas :: DoubleSatDelta -> DoubleSatDelta -> Bool
    sameDeltas (i, o, m) (i', o', m') = i == i' && sameMultiSet o o' && m == m'
      where
        sameMultiSet :: Eq a => [a] -> [a] -> Bool
        sameMultiSet [] [] = True
        sameMultiSet (x : xs) ys = sameMultiSet xs (delete x ys)
        sameMultiSet _ _ = False

    allCombinations :: [[x]] -> [[x]]
    allCombinations (l : ls) = let cs = allCombinations ls in concatMap (\x -> (x :) <$> cs) l
    allCombinations [] = [[]]

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show, Ord)
