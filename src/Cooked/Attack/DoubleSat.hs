{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides an automated attack to try and perform double
-- satisfaction on a contract.
module Cooked.Attack.DoubleSat
  ( DoubleSatDelta,
    DoubleSatLbl (..),
    doubleSatAttack,
  )
where

import Cooked.MockChain.BlockChain
import Cooked.Pretty
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Wallet
import Data.Map (Map)
import Data.Map qualified as Map
import Optics.Core
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

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

-- | A triplet of transaction inputs, transaction outputs, and minted
-- value. This is what we can add to the transaction in order to try a double
-- satisfaction attack.
type DoubleSatDelta = (Map Api.TxOutRef TxSkelRedeemer, [TxSkelOut], TxSkelMints)

instance {-# OVERLAPPING #-} Semigroup DoubleSatDelta where
  (i, o, m) <> (i', o', m') =
    ( i <> i', -- this is left-biased union
      o ++ o',
      m <> m' -- see the 'Semigroup' instance of 'TxSkelMints'
    )

instance {-# OVERLAPPING #-} Monoid DoubleSatDelta where
  mempty = (Map.empty, [], mempty)

-- | Double satisfaction attack. See the comment above for what such an
-- attack is about conceptually.
--
-- This attack consists in adding some extra constraints to a transaction, and
-- hoping that the additional minting policies or validator scripts thereby
-- involved are fooled by what's already present on the transaction. Any extra
-- value contained in new inputs to the transaction is then paid to the
-- attacker.
doubleSatAttack ::
  (MonadTweak m, Eq is, Is k A_Traversal) =>
  -- | how to combine modifications from caused by different foci. See the
  -- comment at 'combineModsTweak', which uses the same logic.
  ([is] -> [[is]]) ->
  -- | Each focus of this optic is a potential reason to add some extra
  -- constraints.
  Optic' k (WithIx is) TxSkel a ->
  -- | How to change each focus, and which inputs, outputs, and mints to add,
  -- for each of the foci. There might be different options for each focus,
  -- that's why the return value is a list.
  --
  -- Continuing the example, for each of the focused script outputs, you might
  -- want to try adding some script inputs to the transaction. Since it might be
  -- interesting to try different redeemers on these extra script inputs, you
  -- can just provide a list of all the options you want to try adding for a
  -- given script output that's already on the transaction.
  --
  -- ###################################
  --
  -- ATTENTION: If you modify the state while computing these lists, the
  -- behaviour of the 'doubleSatAttack' might be strange: Any modification of
  -- the state that happens on any call to this function will be applied to all
  -- returned transactions. For example, if you
  -- 'Cooked.MockChain.BlockChain.awaitSlot' in any of these computations, the
  -- 'doubleSatAttack' will wait for all returned transactions.
  --
  -- TODO: Make this interface safer, for example by using (some kind of) an
  -- 'Cooked.MockChain.UtxoState.UtxoState' argument.
  --
  -- ###################################
  (is -> a -> m [(a, DoubleSatDelta)]) ->
  -- | The wallet of the attacker, where any surplus is paid to.
  --
  -- In the example, the extra value in the added input will be paid to the
  -- attacker.
  Wallet ->
  m ()
doubleSatAttack groupings optic change attacker = do
  deltas <- combineModsTweak groupings optic change
  let delta = joinDoubleSatDeltas deltas
  addDoubleSatDeltaTweak delta
  addedValue <- deltaBalance delta
  if addedValue `Api.gt` mempty
    then addOutputTweak $ attacker `receives` Value addedValue
    else failingTweak
  addLabelTweak DoubleSatLbl
  where
    -- for each triple of additional inputs, outputs, and mints,
    -- calculate its balance
    deltaBalance :: (MonadTweak m) => DoubleSatDelta -> m Api.Value
    deltaBalance (inputs, outputs, mints) = do
      inValue <- foldMap (view (txSkelOutValueL % txSkelOutValueContentL) . snd) . filter ((`elem` Map.keys inputs) . fst) <$> allUtxos
      return $ inValue <> PlutusTx.negate outValue <> mintValue
      where
        outValue = foldOf (traversed % txSkelOutValueL % txSkelOutValueContentL) outputs
        mintValue = txSkelMintsValue mints

    -- Helper tweak to add a 'DoubleSatDelta' to a transaction
    addDoubleSatDeltaTweak :: (MonadTweak m) => DoubleSatDelta -> m ()
    addDoubleSatDeltaTweak (ins, outs, mints) =
      mapM_ (uncurry addInputTweak) (Map.toList ins)
        >> mapM_ addOutputTweak outs
        >> mapM_ addMintTweak (txSkelMintsToList mints)

    -- Join a list of 'DoubleSatDelta's into one 'DoubleSatDelta' that specifies
    -- eveything that is contained in the input.
    joinDoubleSatDeltas :: [DoubleSatDelta] -> DoubleSatDelta
    joinDoubleSatDeltas = mconcat

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- the 'doubleSatAttack'
data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show, Ord)

instance PrettyCooked DoubleSatLbl where
  prettyCooked _ = "DoubleSat"
