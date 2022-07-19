{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cooked.Attack.DoubleSat where

import Cooked.Attack.Common
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
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

The double satisfaction attack 'doubleSatAttack' provided by this module changes
a transaction by adding one or more extra UTxOs belonging to a given script to
the inputs of the transaction. I will try to redeem each of these extra UTxOs
with a list of redeemers, which depend on a 'SpendsScript' constraint in the
original 'TxSkel' and value and datum of the extra UTxO. This makes it so that
the 'txInValue' of the modified transaction will be greater than that of the
original transaction, and that surplus will be paid to the attacker's wallet.

-}

-- | Parameters for a double satisfaction attack.
data DoubleSatParams b = DoubleSatParams
  { -- | The script from which to spend extra inputs.
    dsExtraInputOwner :: L.TypedValidator b,
    -- | For every 'SpendsScriptConstraint' in the original transaction, go
    -- through all @(SpendableOut, L.DatumType b)@ pairs describing UTxOs
    -- belonging to the 'dsExtraInputOwner', and try to redeem them with the
    -- returned list of redeemers.
    dsExtraInputSelect :: SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b],
    -- | The wallet to which any surplus will be paid
    dsAttacker :: Wallet,
    -- | How to add extra inputs: One at a time, all at the same time... There
    -- are currently two options:
    --
    -- - With @dsSplitStrategy = OneChange@, each combination of extra UTxO and
    --   redeemer is tried on a separate modified 'TxSkel': Each of the output
    --   modified 'TxSkel's will consume exactly one extra input.
    --
    -- - With @dsSplitStrategy = AllCombinations@, all modified 'TxSkels' that
    --   consume at least one extra UTxO are tried. That is, if there are n
    --   'SpendsScript' constraints in the original transaction, the modified
    --   transactions will consume at most n extra inputs (and least one). Each
    --   possible combination of extra UTxO and redeemer for the given
    --   'SpendsScript' constraint is tried together with all options for all
    --   other 'SpendsScript' constraints. This means that all cases that the
    --   'OneChange'-case checks are also explored here, plus some (potentially
    --   very many) more.
    --
    -- See the comments for 'mkSplittingAttack' for more explanation on
    -- 'SplitStrategy'.
    dsSplitStrategy :: SplitStrategy
  }

-- | Double satisfaction attack. See the comments for 'DoubleSatParams' for
-- explanation.
doubleSatAttack ::
  forall b.
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  DoubleSatParams b ->
  Attack
doubleSatAttack DoubleSatParams {..} mcst skel =
  mkSplittingAttack
    dsSplitStrategy
    spendsScriptConstraintsT
    ( \_ ssc ->
        map
          (ssc,)
          (extraSpendsScriptConstraints (dsExtraInputSelect ssc))
    )
    (\cs -> addLabel DoubleSatLbl . paySurplusTo dsAttacker . over miscConstraintsL (cs ++))
    mcst
    skel
  where
    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w skelNew = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) skelNew
      where
        -- TODO: how should we compute the surplus? -- Like this:
        surplus = txSkelInValue skelNew <> Pl.negate (txSkelInValue skel)
    -- or like this (which would automatically balance the transaction):
    -- surplus = txSkelOutValue skelNew <> Pl.negate (txSkelInValue skelNew)

    -- Generate all possible 'SpendsScript' constraints using UTxOs that belong
    -- to the 'dsExtraInputOwner' and that are not already being spent in the
    -- original transaction. We try to redeem each of these UTxOs with the list
    -- of redeemers calculated by the passed function.
    extraSpendsScriptConstraints ::
      ((SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
      [MiscConstraint]
    extraSpendsScriptConstraints redeemers =
      concatMap (\utxo -> map (\r -> SpendsScript dsExtraInputOwner r utxo) (redeemers utxo)) $
        filter (\(spOut, _) -> spOut `notElem` currentSpOuts) $
          scriptUtxosSuchThatMcst mcst dsExtraInputOwner (\_ _ -> True)
      where
        currentSpOuts = view (partsOf $ spendsScriptConstraintsT % spendableOutL) skel

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
