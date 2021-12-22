{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cooked.Tx.Constraints.Type where

import Cooked.MockChain.Wallet
import Data.Kind (Type)
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator)
import qualified PlutusTx as Pl

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

-- | Accesses the 'Pl.Value' within a 'SpendableOut'
sOutValue :: SpendableOut -> Pl.Value
sOutValue = Pl.txOutValue . Pl.toTxOut . snd

-- | Accesses the 'Pl.Address' within a 'SpendableOut'
sOutAddress :: SpendableOut -> Pl.Address
sOutAddress = Pl.txOutAddress . Pl.toTxOut . snd

-- | Accesses a potential 'Pl.DatumHash' within a 'SpendableOut'; note that
--  the existence (or not) of a datum hash /DOES NOT/ indicate the 'SpendableOut'
--  belongs to a script or a public key, you must pattern match on the result of
--  'sOutAddress' or use one of 'sBelongsToPubKey' or 'sBelongsToScript' to distinguish that.
sOutDatumHash :: SpendableOut -> Maybe Pl.DatumHash
sOutDatumHash = Pl.txOutDatum . Pl.toTxOut . snd

-- | If a 'SpendableOut' belongs to a public key, return its hash.
sBelongsToPubKey :: SpendableOut -> Maybe Pl.PubKeyHash
sBelongsToPubKey s = case Pl.addressCredential (sOutAddress s) of
  Pl.PubKeyCredential pkh -> Just pkh
  _ -> Nothing

-- | If a 'SpendableOut' belongs to a validator, return its hash.
sBelongsToScript :: SpendableOut -> Maybe Pl.ValidatorHash
sBelongsToScript s = case Pl.addressCredential (sOutAddress s) of
  Pl.ScriptCredential sh -> Just sh
  _ -> Nothing

data CtrFeature = Multisign

type family Elem (f :: CtrFeature) (features :: [CtrFeature]) :: Bool where
  Elem f '[] = 'False
  Elem f (f ': fs) = 'True
  Elem f (a ': fs) = Elem f fs

-- | Our own first-class constraint type. The advantage over the regular plutus constraint
--  type is that we get to add whatever we need and we hide away the type variables in existentials.
data Constraint (features :: [CtrFeature]) :: Type where
  PaysScript ::
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a)) =>
    Pl.TypedValidator a ->
    [(Pl.DatumType a, Pl.Value)] ->
    Constraint features
  SpendsScript ::
    ( Pl.ToData (Pl.DatumType a),
      Pl.ToData (Pl.RedeemerType a),
      Show (Pl.DatumType a),
      Show (Pl.RedeemerType a)
    ) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    Constraint features
  -- TODO: something like stepscript below could be nice!
  -- StepsScript  :: (Pl.ToData a, Pl.ToData redeemer)
  --              => Pl.Validator -> redeemer -> (SpendableOut, a) -> (a -> a) -> Constraint

  PaysPK :: Pl.PubKeyHash -> Pl.Value -> Constraint features
  SpendsPK :: SpendableOut -> Constraint features
  Mints ::
    (Pl.ToData a, Show a) =>
    Maybe a ->
    [Pl.MintingPolicy] ->
    Pl.Value ->
    Constraint features
  Before :: Pl.POSIXTime -> Constraint features
  After :: Pl.POSIXTime -> Constraint features
  ValidateIn :: Pl.POSIXTimeRange -> Constraint features
  SignedBy :: 'Multisign `Elem` features ~ 'True => [Wallet] -> Constraint features

mints :: [Pl.MintingPolicy] -> Pl.Value -> Constraint features
mints = Mints @() Nothing

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
data TxSkel features where
  TxSkel ::
    (Show x) =>
    { txLabel :: Maybe x,
      txMainSigner :: Wallet,
      txConstraints :: [Constraint features]
    } ->
    TxSkel features

-- | Constructs a skeleton without a default label
txSkel :: Wallet -> [Constraint features] -> TxSkel features
txSkel = TxSkel @() Nothing

-- | Constructs a skeleton with a label
txSkelLbl :: (Show x) => x -> Wallet -> [Constraint features] -> TxSkel features
txSkelLbl x = TxSkel (Just x)
