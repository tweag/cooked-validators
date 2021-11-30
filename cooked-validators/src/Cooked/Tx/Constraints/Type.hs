{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Constraints.Type where

import Cooked.MockChain.Wallet
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator)
import qualified PlutusTx as Pl

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

-- | Our own first-class constraint type. The advantage over the regular plutus constraint
--  type is that we get to add whatever we need and we hide away the type variables in existentials.
data Constraint where
  PaysScript ::
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a)) =>
    Pl.TypedValidator a ->
    [(Pl.DatumType a, Pl.Value)] ->
    Constraint
  SpendsScript ::
    ( Pl.ToData (Pl.DatumType a),
      Pl.ToData (Pl.RedeemerType a),
      Show (Pl.DatumType a),
      Show (Pl.RedeemerType a)
    ) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    Constraint
  -- TODO: something like stepscript below could be nice!
  -- StepsScript  :: (Pl.ToData a, Pl.ToData redeemer)
  --              => Pl.Validator -> redeemer -> (SpendableOut, a) -> (a -> a) -> Constraint

  PaysPK :: Pl.PubKeyHash -> Pl.Value -> Constraint
  SpendsPK :: SpendableOut -> Constraint
  Mints :: [Pl.MintingPolicy] -> Pl.Value -> Constraint
  Before :: Pl.POSIXTime -> Constraint
  After :: Pl.POSIXTime -> Constraint
  ValidateIn :: Pl.POSIXTimeRange -> Constraint
  SignedBy :: [Wallet] -> Constraint

-- | A Transaction skeleton is a set of our constraints,
-- a wallet which will sign the generated transaction and
-- an optional showable label, which is very useful when displaying
-- traces. The label was encoded as an existential to enable us to
-- create ad-hoc datatypes and pass their values around, for instance:
--
-- > data ProposePaymentLbl = ProposePayment Wallet Value deriving Show
-- > f = ... (txSkelLbl (ProposePayment w val) ...) ...
--
-- This way we can (A) modify the show behavior if we want and (B)
-- not worry about constructing consistent strings when constructing transactions
data TxSkel where
  TxSkel ::
    (Show x) =>
    { txLabel :: Maybe x,
      txMainSigner :: Wallet,
      txConstraints :: [Constraint]
    } ->
    TxSkel

-- | Constructs a skeleton without a default label
txSkel :: Wallet -> [Constraint] -> TxSkel
txSkel = TxSkel @() Nothing

-- | Constructs a skeleton with a label
txSkelLbl :: (Show x) => x -> Wallet -> [Constraint] -> TxSkel
txSkelLbl x = TxSkel (Just x)
