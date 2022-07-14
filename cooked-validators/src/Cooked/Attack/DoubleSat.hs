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

-- | Parameters for a double satisfaction attack. A double satisfaction attack
-- consists in adding one or more 'SpendsScript' constraints to an otherwise
-- normal transaction, hoping that the existing other constraints already make
-- the validator of the additional constraint accept the transaction. Then, the
-- value contained in the additional consumed script UTxO(s) can be spent to an
-- attacker.
data DoubleSatParams b = DoubleSatParams
  { -- | The script from which to spend extra inputs.
    dsExtraInputOwner :: L.TypedValidator b,
    -- | For every 'SpendsScriptConstraint' in the original transaction, go
    -- through all @(SpendableOut, L.DatumType b)@ pairs describing UTxOs
    -- belonging to the 'dsExtraInputOwner', and try to redeem them with the
    -- returned list of redeemers
    dsExtraInputSelect :: SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b],
    -- | The wallet to which any surplus will be paid
    dsAttacker :: Wallet,
    -- | How to add extra inputs: One at a time, all at the same time... See the
    -- comments for 'mkSplittingAttack'.
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
    paySurplusTo w s = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) s
      where
        surplus = txSkelInValue s <> Pl.negate (txSkelInValue skel)

    -- Generate all possible 'SpendsScript' constraints using UTxOs belonging to
    -- the given validator, where we try to redeem each of these UTxOs with the
    -- list of redeemers returned by the second argument.
    extraSpendsScriptConstraints ::
      ((SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
      [MiscConstraint]
    extraSpendsScriptConstraints redeemers =
      concatMap (\utxo -> map (\r -> SpendsScript dsExtraInputOwner r utxo) (redeemers utxo)) $
        scriptUtxosSuchThatMcst mcst dsExtraInputOwner (\_ _ -> True)

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
