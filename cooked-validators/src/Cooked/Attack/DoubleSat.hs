{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cooked.Attack.DoubleSat where

import Cooked.Attack.Common
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Bifunctor
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

{- Note: What is a double satisfaction attack?

A double satisfaction attack consists in trying to satisfy the requirements for
what conceptually are two transactions in a single transaction, and doing so
incompletely. It succeeds whenever the requirements of two validators (which
often are the same) ovelap, but the outputs of the transaction are not
sufficiently unique, so that both validators see the outputs as satisfying
"their" requirement.

The mechanism is explained very well in the following analogy from the Plutus
documentation: "Suppose that two tax auditors from two different departments
come to visit you in turn to see if you’ve paid your taxes. You come up with a
clever scheme to confuse them. Your tax liability to both departments is $10, so
you make a single payment to the tax office’s bank account for $10. When the
auditors arrive, you show them your books, containing the payment to the tax
office. They both leave satisfied."

The double satisfaction attack 'doubleSatAttack' provided by this module works
by adding some extra 'Constraints' that depend on 'SpendsScript' constraints on
the transaction under modification and the current 'MockChainSt'ate. This very
general concept allows the implementation of common double satisfaction patterns
as special cases, which will correspond to smart constructors for the
'DoubleSatParams' type.
-}

-- | Parameters for a double satisfaction attack. You probably don't want to
-- initialise these by hand; better use one of the smart constructors defined
-- below.
data DoubleSatParams = DoubleSatParams
  { -- | For every 'SpendsScriptConstraint' in the transaction under
    -- modification, calculate a list of extra 'Constraints' to add to the
    -- transaction in order to try a double satisfaction attack.
    --
    -- Extra 'Constraints' like @extraIs :=>: extraOs@ are added to pre-existing
    -- 'Constraints' @is :=>: os@ as follows:
    --
    -- > (extraIs ++ is) :=>: (os :=>: extraOs)
    --
    -- while the order on the left of ':=>:' shouldn't matter, the order on the
    -- right is chosen deliberately because contracts may rely on the ordering
    -- transaction outputs. If you want to try all permutations on the right,
    -- look at the function 'tryOutPermutations', which is an attack-agnostic
    -- way to accomplish that.
    dsExtraConstraints :: MockChainSt -> SpendsScriptConstraint -> [Constraints],
    -- | The wallet to which any surplus will be paid.
    dsAttacker :: Wallet,
    -- | How to combine extra 'Constraint's:
    --
    -- There are at the moment the following two strategies for how to combine
    -- additional 'Constraints' belonging to _different_ 'SpendsScript'
    -- constraints of the original transaction.
    --
    -- - With @dsSplitStrategy = OneChange@, each extra 'Constraints' is added
    --   onto a separate modified 'TxSkel': Each of the output modified
    --   'TxSkel's will receive exactly one of the extra 'Constraints'.
    --
    -- - With @dsSplitStrategy = AllCombinations@, al 'TxSkels' obtained by
    --   adding _at least one_ extra 'Constraints' belonging to an original
    --   'SpendsScript' constraints are tried: If there are n 'SpendsScript'
    --   constraints in the original transaction, the modified transactions will
    --   incorporate up to n extra 'Constraints'. For a given 'SpendsScript'
    --   constraint on the original transaction, each of its extra 'Constraints'
    --   is tried together with all options for all other 'SpendsScript'
    --   constraints. This means that all cases that the 'OneChange'-case checks
    --   are also explored here, plus some (potentially very many) more.
    --
    -- See the comments for 'mkSplittingAttack' for more explanation on
    -- 'SplitStrategy'.
    dsSplitStrategy :: SplitStrategy
  }

-- | Parameters for a double satisfaction attack that takes the following
-- perspective: "I hav set of traces that I want to use to test whether a given,
-- fixed validator is vulnerable to a double satisfaction attack"
--
-- TODO more explanation/better interface
doubleSatParamsAddSingleFrom ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  L.TypedValidator b ->
  (SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  Wallet ->
  SplitStrategy ->
  DoubleSatParams
doubleSatParamsAddSingleFrom extraInputOwner extraInputRedeemers attacker splitStrategy =
  DoubleSatParams
    { dsExtraConstraints = \mcst ssc ->
        let extraUtxos = scriptUtxosSuchThatMcst mcst extraInputOwner (\_ _ -> True)
         in concatMap
              ( \utxo ->
                  map
                    (\r -> toConstraints $ SpendsScript extraInputOwner r utxo)
                    (extraInputRedeemers ssc utxo)
              )
              extraUtxos,
      dsAttacker = attacker,
      dsSplitStrategy = splitStrategy
    }

-- | Double satisfaction attack. See the comments for 'DoubleSatParams' and its
-- smart constructors for explanation.
doubleSatAttack ::
  DoubleSatParams ->
  Attack
doubleSatAttack DoubleSatParams {..} =
  mkSplittingAttack
    dsSplitStrategy
    spendsScriptConstraintsT
    ( \mcst ssc ->
        map
          (ssc,)
          (dsExtraConstraints mcst ssc)
    )
    ( \extraConstrs skelOld ->
        addLabel DoubleSatLbl $
          paySurplusTo dsAttacker $
            over txConstraintsL (accConstraints extraConstrs) skelOld
    )
  where
    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w s = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) s
      where
        surplus = txSkelInValue s <> Pl.negate (txSkelOutValue s)

    -- Accumulate a list of 'Constraints' into a single 'Constraints', in such a
    -- way that extra 'MiscConstraint's are added in the front (the order should
    -- not matter), and extra 'OutConstraint's are added in the back (here, the
    -- order might matter).
    --
    -- Note for the future: Adding constraints in this way is dangerous: We
    -- might for example add two constraints that try to spend the same UTxO
    -- with different redeemers.
    accConstraints :: [Constraints] -> Constraints -> Constraints
    accConstraints [] acc = acc
    accConstraints ((is :=>: os) : cs) acc =
      over constraintPairI (bimap (is ++) (++ os)) $
        accConstraints cs acc

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
