{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cooked.Attack.DoubleSat where

import Cooked.Attack.Common
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

data DoubleSatParams b = DoubleSatParams
  { dsExtraInputOwner :: L.TypedValidator b,
    -- | For every 'SpendsScriptConstraint' in the original transaction, go
    -- through all @(SpendableOut, L.DatumType b)@ pairs describing UTxOs
    -- belonging to the 'dsExtraInputOwner', and try to redeem them with the
    -- returned list of redeemers
    dsExtraInputSelect :: SpendsScriptConstraint -> SpendableOut -> L.DatumType b -> [L.RedeemerType b],
    dsAttacker :: Wallet
  }

doubleSatAttack ::
  forall b.
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  DoubleSatParams b ->
  Attack
doubleSatAttack DoubleSatParams {..} mcst skel =
  mkAccumLAttack
    spendsScriptConstraintsT
    ( \acc ssc ->
        ( ssc,
          possibleSpendsScriptConstraints
            mcst
            dsExtraInputOwner
            (uncurry $ dsExtraInputSelect ssc)
            ++ acc
        )
    )
    []
    ( \_ sk acc ->
        map
          ( \ssc ->
              addLabel DoubleSatLbl
                . paySurplusTo dsAttacker
                $ over miscConstraintsL (ssc :) sk
          )
          acc
    )
    mcst
    skel
  where
    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w sk = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) sk
      where
        surplus = txSkelInValue sk <> Pl.negate (txSkelInValue skel)

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)

-- | Generate all possible 'SpendsScript' constraints using UTxOs belonging to
-- the given validator, where we try to redeem each of these UTxOs with the list
-- of redeemers returned by the second argument.
possibleSpendsScriptConstraints ::
  ( SpendsConstrs a,
    Pl.FromData (L.DatumType a)
  ) =>
  MockChainSt ->
  L.TypedValidator a ->
  ((SpendableOut, L.DatumType a) -> [L.RedeemerType a]) ->
  [MiscConstraint]
possibleSpendsScriptConstraints mcst val redeemers =
  concatMap (\o -> map (\r -> SpendsScript val r o) (redeemers o)) $
    scriptUtxosSuchThatMcst mcst val (\_ _ -> True)
