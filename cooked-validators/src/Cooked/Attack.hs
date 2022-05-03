{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.Attack where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Maybe
import qualified Ledger as Pl hiding (validatorHash)
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified PlutusTx.AssocMap as Pl
import qualified PlutusTx.IsData as Pl
import qualified PlutusTx.Numeric as Pl (negate)
import qualified PlutusTx.TH as Pl
import Type.Reflection
import Debug.Trace
import qualified PlutusTx as Pl

-- * The type of attacks, and functions to turn optics into attacks

type Attack = TxSkel -> Maybe TxSkel

-- | The simplest way to make an attack from an optic: Apply some function to
-- all of its foci, of fail returning @Nothing@ if nothing is in focus.
mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> a) -> Attack
mkAttack = failover

-- Optic' k is s a == Optic k is s s a a

mkAttack' :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> Maybe a) -> Attack
mkAttack' = traverseOf

-- * Some optics on 'TxSkel's, 'Constraint's, etc.

-- There's a recurring pattern here to work around the existential type
-- variables in the constructors in 'Cooked.Tx.Constraints.Type': Define a type
-- that corresponds to the image of the constructor an then use that type as the
-- "codomain" of the optics.

-- ** Picking apart 'TxSkel's

data TxLabel where
  TxLabel :: LabelConstrs x => Maybe x -> TxLabel

txLabelL :: Lens' TxSkel TxLabel
txLabelL =
  lens
    (\(TxSkel l _ _) -> TxLabel l)
    (\(TxSkel _ o c) (TxLabel l) -> TxSkel l o c)

txOptsL :: Lens' TxSkel TxOpts
txOptsL = lens txOpts (\(TxSkel l _ c) o -> TxSkel l o c)

txConstraintsL :: Lens' TxSkel Constraints
txConstraintsL =
  lens
    txConstraints
    (\(TxSkel l o _) c -> TxSkel l o c)

constraintPair :: Iso' Constraints ([MiscConstraint], [OutConstraint])
constraintPair = iso (\(i :=>: o) -> (i, o)) (uncurry (:=>:))

outConstraintsL :: Lens' TxSkel [OutConstraint]
outConstraintsL = txConstraintsL % constraintPair % _2

miscConstraintsL :: Lens' TxSkel [MiscConstraint]
miscConstraintsL = txConstraintsL % constraintPair % _1

-- ** Picking apart 'MiscConstraint's

data MintsConstraint where
  MintsConstraint ::
    MintsConstrs a =>
    Maybe a ->
    [Pl.MintingPolicy] ->
    Pl.Value ->
    MintsConstraint

mintsConstraintP :: Prism' MiscConstraint MintsConstraint
mintsConstraintP =
  prism'
    ( \case
        MintsConstraint r ps v -> Mints r ps v
    )
    ( \case
        Mints r ps v -> Just $ MintsConstraint r ps v
        _ -> Nothing
    )

mintsConstraintsT :: Traversal' TxSkel MintsConstraint
mintsConstraintsT = miscConstraintsL % traversed % mintsConstraintP

data SpendsScriptConstraint where
  SpendsScriptConstraint ::
    (SpendsConstrs a) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    SpendsScriptConstraint

spendsScriptConstraintP :: Prism' MiscConstraint SpendsScriptConstraint
spendsScriptConstraintP =
  prism'
    ( \case
        SpendsScriptConstraint v r o -> SpendsScript v r o
    )
    ( \case
        SpendsScript v r o -> Just $ SpendsScriptConstraint v r o
        _ -> Nothing
    )

spendableOutL :: Lens' SpendsScriptConstraint SpendableOut
spendableOutL =
  lens
    ( \case
        SpendsScriptConstraint _ _ (o, _) -> o
    )
    ( \c o -> case c of
        SpendsScriptConstraint v r (_, d) -> SpendsScriptConstraint v r (o, d)
    )

spendsPKConstraintP :: Prism' MiscConstraint SpendableOut
spendsPKConstraintP =
  prism'
    SpendsPK
    ( \case
        SpendsPK o -> Just o
        _ -> Nothing
    )

-- ** Picking apart 'OutConstraint's

data PaysScriptConstraint where
  PaysScriptConstraint ::
    PaysScriptConstrs a =>
    Pl.TypedValidator a ->
    Pl.DatumType a ->
    Pl.Value ->
    PaysScriptConstraint

paysScriptConstraintP :: Prism' OutConstraint PaysScriptConstraint
paysScriptConstraintP =
  prism'
    ( \case
        PaysScriptConstraint v d x -> PaysScript v d x
    )
    ( \case
        PaysScript v d x -> Just $ PaysScriptConstraint v d x
        _ -> Nothing
    )

paysScriptConstraintsT :: Traversal' TxSkel PaysScriptConstraint
paysScriptConstraintsT = outConstraintsL % traversed % paysScriptConstraintP

-- ** Extracting 'Pl.Value's from different types

class HasValue a where
  valueL :: Lens' a Pl.Value

instance HasValue Pl.ChainIndexTxOut where
  valueL =
    lens
      ( \case
          Pl.PublicKeyChainIndexTxOut _ x -> x
          Pl.ScriptChainIndexTxOut _ _ _ x -> x
      )
      ( \o x -> case o of
          Pl.PublicKeyChainIndexTxOut a _ -> Pl.PublicKeyChainIndexTxOut a x
          Pl.ScriptChainIndexTxOut a v d _ -> Pl.ScriptChainIndexTxOut a v d x
      )

instance HasValue SpendableOut where
  valueL = _2 % valueL

instance HasValue OutConstraint where
  valueL =
    lens
      ( \case
          PaysScript _ _ v -> v
          PaysPKWithDatum _ _ _ v -> v
      )
      ( \c x -> case c of
          PaysScript v d _ -> PaysScript v d x
          PaysPKWithDatum h sh d _ -> PaysPKWithDatum h sh d x
      )

instance HasValue MintsConstraint where
  valueL =
    lens
      ( \case
          MintsConstraint _ _ x -> x
      )
      ( \c x -> case c of
          MintsConstraint r ps _ -> MintsConstraint r ps x
      )

instance HasValue SpendsScriptConstraint where
  valueL = spendableOutL % valueL

-- | Given two 'AffineTraversal's of the same source and target types, which are
-- disjoint in the sense that at most one of them has a focus at any given
-- input, combine them in to one affine traversal. If the disjointness
-- requirement is violated, this will not be a lawful 'AffineTraversal'.
unsafeOr ::
  (Is k An_AffineTraversal, Is l An_AffineTraversal) =>
  Optic' k is s a ->
  Optic' l js s a ->
  AffineTraversal' s a
unsafeOr o1 o2 = withAffineTraversal o1 $ \m1 u1 ->
  withAffineTraversal o2 $ \m2 u2 ->
    atraversal
      ( \s -> case m1 s of
          Left _ -> case m2 s of
            Left _ -> Left s
            Right a -> Right a
          Right a -> Right a
      )
      (\s a -> u2 (u1 s a) a)

infixr 6 `unsafeOr` -- same fixity as <>

valueAT :: AffineTraversal' MiscConstraint Pl.Value
valueAT =
  (spendsScriptConstraintP % valueL)
    `unsafeOr` (mintsConstraintP % valueL)
    `unsafeOr` (spendsPKConstraintP % valueL)

txSkelInValue :: TxSkel -> Pl.Value
txSkelInValue = foldOf (miscConstraintsL % traversed % valueAT)

txSkelOutValue :: TxSkel -> Pl.Value
txSkelOutValue = foldOf (outConstraintsL % traversed % valueL)

-- * Token duplication attack

-- | A token duplication attack which modifies every 'Mints'-constraint of a
-- 'TxSkel' that satisfies some conditions. This adds a 'DupTokenLbl' to the
-- labels of the transaction using 'addLabel'.
dupTokenAttack ::
  -- | A function describing how the amount of minted tokens of an asset class
  -- should be changed by the attack. The given function @f@ is expected to
  -- satisfy
  -- > f ac i == Just j -> i <= j
  -- for all @ac@ and @i@ (i.e. it should never decrease the minted amount). If
  -- @f ac i == Nothing@, the value will be left unchanged.
  (Pl.AssetClass -> Integer -> Maybe Integer) ->
  -- | The wallet of the attacker. Any extra tokens that were minted by the
  -- transaction are paid to this wallet.
  Wallet ->
  Attack
dupTokenAttack change attacker skel =
  addLabel DupTokenLbl
    . paySurplusTo attacker
    <$> mkAttack (mintsConstraintsT % valueL) increaseValue skel
  where
    increaseValue :: Pl.Value -> Pl.Value
    increaseValue (Pl.Value symbolMap) =
      Pl.Value $
        Pl.mapWithKey
          ( \cs tokenNameMap ->
              Pl.mapWithKey
                ( \tn i -> fromMaybe i $ change (Pl.assetClass cs tn) i
                )
                tokenNameMap
          )
          symbolMap

    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w s = over outConstraintsL (paysPK (walletPKHash w) surplus :) s
      where
        surplus = txSkelInValue s <> Pl.negate (txSkelOutValue s)

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show)

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabel :: LabelConstrs x => x -> TxSkel -> TxSkel
addLabel newlabel =
  over
    txLabelL
    ( \case
        TxLabel Nothing -> TxLabel $ Just newlabel
        TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
    )

-- * Datum hijacking attack

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that the (relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work. A
-- 'DatumHijackingLbl' is added to the labels of the 'TxSkel' using 'addLabel'.
datumHijackingAttack ::
  forall a.
  (Typeable a,
   Pl.UnsafeFromData (Pl.DatumType a),
   Pl.UnsafeFromData (Pl.RedeemerType a)) =>
  -- | Validator script to steal from.
  Pl.TypedValidator a ->
  -- | A function indicating whether to try the attack on a 'PaysScript'
  -- constraint with the given datum and value.
  (Pl.DatumType a -> Pl.Value -> Bool) ->
  Attack
datumHijackingAttack val change skel =
  addLabel DatumHijackingLbl
    <$> mkAttack paysScriptConstraintsT changeRecipient skel
  where
    changeRecipient :: PaysScriptConstraint -> PaysScriptConstraint
    changeRecipient c@(PaysScriptConstraint val' dat money) =
      trace "a" $
       case val' ~*~? val of
        Just HRefl -> trace "found" $
          if Pl.validatorHash val' == Pl.validatorHash val -- && change dat money
          then PaysScriptConstraint @a x dat money
          else c
        Nothing -> c

data DatumHijackingLbl = DatumHijackingLbl
  deriving (Show, Eq)

-- The trivial validator that always returns @True@
datumHijackingTarget ::
  forall a.
  ( Typeable a,
    Pl.UnsafeFromData (Pl.DatumType a),
    Pl.UnsafeFromData (Pl.RedeemerType a)
  ) =>
  Pl.TypedValidator a
datumHijackingTarget =
  Pl.mkTypedValidator @a
    $$(Pl.compile [||\_ _ _ -> True||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.wrapValidator @(Pl.DatumType a) @(Pl.RedeemerType a)


datumHijackingTarget' :: Pl.Validator
datumHijackingTarget' = Pl.mkValidatorScript
  $$(Pl.compile [||\_ _ _ -> ()||])

x :: Pl.TypedValidator a
x = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||datumHijackingTgt ||]))
 where
  datumHijackingTgt ::
   Pl.BuiltinData ->
   Pl.BuiltinData ->
   Pl.BuiltinData ->
   ()
  datumHijackingTgt _ _ _ = ()

-- scriptRecipientP :: Pl.TypedValidator a -> Prism' PaysScriptConstraint (Pl.DatumType a, Pl.Value)
