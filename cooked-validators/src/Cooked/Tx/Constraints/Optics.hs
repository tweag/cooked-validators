{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Various Optics on 'TxSkels', constraints, and all the other types defined
-- in 'Cooked.Tx.Constraints.Type'.
module Cooked.Tx.Constraints.Optics where

import Cooked.Tx.Constraints.Type
import qualified Ledger as L
import qualified Ledger.Ada as L
import qualified Ledger.Credential as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Type.Reflection

-- A few remarks:

-- There's a recurring pattern here to work around the existential type
-- variables in the constructors in "Cooked.Tx.Constraints.Type": Define a type
-- that corresponds to the image of the constructor an then use that type as the
-- target of the optics. So, if you find a single-constructor type in this
-- module that you would like some explanation for, look at the comments for the
-- types 'MiscConstraint' and 'OutConstraint' in "Cooked.Tx.Constraints.Type".

-- The naming convention for optics in this module is as follows: Lenses have
-- names that end in 'L', Prisms end in 'P', traversals in 'T', affine
-- traversals in 'AT', isos in 'I'. There are not yet and optics of other types
-- here.

-- * Picking apart 'TxSkel's

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

constraintPairI :: Iso' Constraints ([MiscConstraint], [OutConstraint])
constraintPairI = iso (\(i :=>: o) -> (i, o)) (uncurry (:=>:))

outConstraintsL :: Lens' TxSkel [OutConstraint]
outConstraintsL = txConstraintsL % constraintPairI % _2

outConstraintT :: Traversal' TxSkel OutConstraint
outConstraintT = outConstraintsL % traversed

miscConstraintsL :: Lens' TxSkel [MiscConstraint]
miscConstraintsL = txConstraintsL % constraintPairI % _1

miscConstraintT :: Traversal' TxSkel MiscConstraint
miscConstraintT = miscConstraintsL % traversed

-- * Picking apart 'MiscConstraint's

data MintsConstraint where
  MintsConstraint ::
    MintsConstrs a =>
    Maybe a ->
    [L.MintingPolicy] ->
    L.Value ->
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
mintsConstraintsT = miscConstraintT % mintsConstraintP

data SpendsScriptConstraint where
  SpendsScriptConstraint ::
    (SpendsConstrs a) =>
    L.TypedValidator a ->
    L.RedeemerType a ->
    SpendableOut ->
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

-- | This prism only has a focus on 'SpendsScriptConstraint's of a given type.
spendsScriptConstraintTypeP ::
  forall a.
  SpendsConstrs a =>
  Prism' SpendsScriptConstraint (L.TypedValidator a, L.RedeemerType a, SpendableOut)
spendsScriptConstraintTypeP =
  prism'
    (\(v, r, o) -> SpendsScriptConstraint v r o)
    ( \(SpendsScriptConstraint v r o) ->
        case typeOf v `eqTypeRep` typeRep @(L.TypedValidator a) of
          Just HRefl -> Just (v, r, o)
          Nothing -> Nothing
    )

spendsScriptConstraintsT :: Traversal' TxSkel SpendsScriptConstraint
spendsScriptConstraintsT = miscConstraintT % spendsScriptConstraintP

spendableOutL :: Lens' SpendsScriptConstraint SpendableOut
spendableOutL =
  lens
    ( \case
        SpendsScriptConstraint _ _ o -> o
    )
    ( \c o -> case c of
        SpendsScriptConstraint v r _ -> SpendsScriptConstraint v r o
    )

spendsPKConstraintP :: Prism' MiscConstraint SpendableOut
spendsPKConstraintP =
  prism'
    SpendsPK
    ( \case
        SpendsPK o -> Just o
        _ -> Nothing
    )

signedByP :: Prism' MiscConstraint [L.PubKeyHash]
signedByP =
  prism'
    SignedBy
    ( \case
        SignedBy signers -> Just signers
        _ -> Nothing
    )

beforeP :: Prism' MiscConstraint L.POSIXTime
beforeP =
  prism'
    Before
    ( \case
        Before b -> Just b
        _ -> Nothing
    )

afterP :: Prism' MiscConstraint L.POSIXTime
afterP =
  prism'
    After
    ( \case
        After b -> Just b
        _ -> Nothing
    )

validateInP :: Prism' MiscConstraint L.POSIXTimeRange
validateInP =
  prism'
    ValidateIn
    ( \case
        ValidateIn r -> Just r
        _ -> Nothing
    )

-- * Picking apart 'OutConstraint's

data PaysScriptConstraint where
  PaysScriptConstraint ::
    PaysScriptConstrs a =>
    L.TypedValidator a ->
    Maybe L.StakingCredential ->
    L.DatumType a ->
    L.Value ->
    PaysScriptConstraint

paysScriptConstraintP :: Prism' OutConstraint PaysScriptConstraint
paysScriptConstraintP =
  prism'
    ( \case
        PaysScriptConstraint v sc d x -> PaysScript v sc d x
    )
    ( \case
        PaysScript v sc d x -> Just $ PaysScriptConstraint v sc d x
        _ -> Nothing
    )

paysScriptConstraintsT :: Traversal' TxSkel PaysScriptConstraint
paysScriptConstraintsT = outConstraintT % paysScriptConstraintP

-- | This prism only has a focus on 'PaysScriptConstraint's of a given type.
paysScriptConstraintTypeP ::
  forall a.
  PaysScriptConstrs a =>
  Prism' PaysScriptConstraint (L.TypedValidator a, Maybe L.StakingCredential, L.DatumType a, L.Value)
paysScriptConstraintTypeP =
  prism'
    (\(v, sc, d, x) -> PaysScriptConstraint v sc d x)
    ( \(PaysScriptConstraint v sc d x) ->
        case typeOf v `eqTypeRep` typeRep @(L.TypedValidator a) of
          Just HRefl -> Just (v, sc, d, x)
          Nothing -> Nothing
    )

data PaysPKWithDatumConstraint where
  PaysPKWithDatumConstraint ::
    (Pl.ToData a, Pl.Eq a, Show a, Typeable a) =>
    L.PubKeyHash ->
    Maybe L.StakePubKeyHash ->
    Maybe a ->
    L.Value ->
    PaysPKWithDatumConstraint

paysPKWithDatumConstraintP :: Prism' OutConstraint PaysPKWithDatumConstraint
paysPKWithDatumConstraintP =
  prism'
    ( \case
        PaysPKWithDatumConstraint h sh d x -> PaysPKWithDatum h sh d x
    )
    ( \case
        PaysPKWithDatum h sh d x -> Just $ PaysPKWithDatumConstraint h sh d x
        _ -> Nothing
    )

paysPKWithDatumConstraintsT :: Traversal' TxSkel PaysPKWithDatumConstraint
paysPKWithDatumConstraintsT = outConstraintT % paysPKWithDatumConstraintP

-- * Extracting 'L.Value's from different types

class HasValue a where
  valueL :: Lens' a L.Value

instance HasValue L.ChainIndexTxOut where
  valueL =
    lens
      ( \case
          L.PublicKeyChainIndexTxOut _ x -> x
          L.ScriptChainIndexTxOut _ _ _ x -> x
      )
      ( \o x -> case o of
          L.PublicKeyChainIndexTxOut a _ -> L.PublicKeyChainIndexTxOut a x
          L.ScriptChainIndexTxOut a v d _ -> L.ScriptChainIndexTxOut a v d x
      )

instance HasValue SpendableOut where
  valueL = _2 % valueL

instance HasValue OutConstraint where
  valueL =
    lens
      ( \case
          PaysScript _ _ _ v -> v
          PaysPKWithDatum _ _ _ v -> v
      )
      ( \c x -> case c of
          PaysScript v sc d _ -> PaysScript v sc d x
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

instance HasValue PaysPKWithDatumConstraint where
  valueL =
    lens
      (\(PaysPKWithDatumConstraint _ _ _ x) -> x)
      (\(PaysPKWithDatumConstraint h msh d _) x -> PaysPKWithDatumConstraint h msh d x)

valueAT :: AffineTraversal' MiscConstraint L.Value
valueAT =
  (spendsScriptConstraintP % valueL)
    `unsafeOr` (mintsConstraintP % valueL)
    `unsafeOr` (spendsPKConstraintP % valueL)
  where
    -- In the best of all possible worlds, I'd write this:
    -- > unsafeOr = singular . adjoin
    -- Alas, @adjoin@ only is available in optics-core >= 0.4, which we can not
    -- use at the moment, because of the compiler and cabal versions from IOHK
    -- that we use (at least that is what I think is going on).
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

-- | The combined value contained in all 'MiscConstraints' of a 'TxSkel'.
txSkelInValue :: TxSkel -> L.Value
txSkelInValue = foldOf (miscConstraintT % valueAT)

-- | The combined value contained in all 'OutConstraints' of a 'TxSkel'.
txSkelOutValue :: TxSkel -> L.Value
txSkelOutValue = foldOf (outConstraintT % valueL)

-- * Picking apart 'Value's

flattenValueI :: Iso' L.Value [(L.AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (L.assetClass cSymbol tName, amount)) . L.flattenValue)
    (foldl (\v (ac, amount) -> v <> L.assetClassValue ac amount) mempty)

-- | The portion of a 'L.Value' that is not Ada.
nonAdaValue :: L.Value -> L.Value
nonAdaValue = over flattenValueI (map $ \(ac, i) -> if ac == adaAssetClass then (ac, 0) else (ac, i))
  where
    adaAssetClass = L.assetClass L.adaSymbol L.adaToken
