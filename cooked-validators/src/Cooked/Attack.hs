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
import qualified PlutusTx as Pl
import qualified PlutusTx.AssocMap as Pl
import qualified PlutusTx.Numeric as Pl (negate)
import Type.Reflection

-- * The type of attacks, and functions to turn optics into attacks

-- | The type of attacks that operate on a single transaction. The idea is to
-- try to modify a transaction, or return @Nothing@ if the modification does not
-- apply; use this with the 'somewhere' and 'everywhere' modalities from
-- 'Cooked.MockChain.Monad'.
type Attack = TxSkel -> Maybe TxSkel

-- | The simplest way to make an attack from an optic: Try to apply the given
-- function to all of its foci. Fail returning @Nothing@ if nothing is in focus
-- or if all foci are evaluated to @Nothing@.
mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> Maybe a) -> Attack
mkAttack = traverseOf

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

-- scriptRecipientL ::  PaysScirptConstraint

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
  -- should be changed by the attack. This function @f@ should probably satisfy
  -- > f ac i == Just j -> i < j
  -- for all @ac@ and @i@, i.e. it should increase in the minted amount.
  -- If it does *not* increase the minted amount, or if @f ac i == Nothing@, the
  -- minted amount will be left unchanged.
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
    increaseValue :: Pl.Value -> Maybe Pl.Value
    increaseValue oldValue@(Pl.Value symbolMap) =
      let newValue =
            Pl.Value $
              Pl.mapWithKey
                ( \cs tokenNameMap ->
                    Pl.mapWithKey
                      ( \tn i -> fromMaybe i $ change (Pl.assetClass cs tn) i
                      )
                      tokenNameMap
                )
                symbolMap
       in if oldValue `Pl.lt` newValue then Just newValue else Nothing

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
  ( Typeable a,
    Pl.UnsafeFromData (Pl.DatumType a),
    Pl.UnsafeFromData (Pl.RedeemerType a)
  ) =>
  -- | Validator script to steal from.
  Pl.TypedValidator a ->
  -- | A function indicating whether to try the attack on a 'PaysScript'
  -- constraint with the given datum and value.
  (Pl.DatumType a -> Pl.Value -> Bool) ->
  Attack
datumHijackingAttack val change skel =
  addLabel DatumHijackingLbl
    <$> mkAttack (partsOf paysScriptConstraintsT) changeRecipient' skel
  where
    -- TODO: this solution with @changeRecipient'@ is not nice. It would be
    -- cleaner to have an traversal that focuses all 'PaysScriptConstraint's
    -- that pay to the validator that we want to steal from an then apply
    -- @changeRecipient@
    changeRecipient' :: [PaysScriptConstraint] -> Maybe [PaysScriptConstraint]
    changeRecipient' = oneJust changeRecipient

    changeRecipient :: PaysScriptConstraint -> Maybe PaysScriptConstraint
    changeRecipient (PaysScriptConstraint val' dat money) =
      case val' ~*~? val of
        Just HRefl ->
          if Pl.validatorHash val' == Pl.validatorHash val && change dat money
            then Just $ PaysScriptConstraint @a datumHijackingTarget dat money
            else Nothing
        Nothing -> Nothing

data DatumHijackingLbl = DatumHijackingLbl
  deriving (Show, Eq)

datumHijackingTarget :: Pl.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||datumHijackingTgt||]))
  where
    datumHijackingTgt ::
      Pl.BuiltinData ->
      Pl.BuiltinData ->
      Pl.BuiltinData ->
      ()
    datumHijackingTgt _ _ _ = ()

-- | If the given function evaluates exactly one element @x@ to @Just y@, return
-- the list with @x@ changed for @y@, else return @Nothing@.
oneJust :: (b -> Maybe b) -> [b] -> Maybe [b]
oneJust _ [] = Nothing
oneJust f (x : xs)
  | Just y <- f x = (y :) <$> allNothing f xs
  | otherwise = (x :) <$> oneJust f xs

-- | return the list, only if all elements are eveluated to @Nothing@ by the
-- given function
allNothing :: (b -> Maybe b) -> [b] -> Maybe [b]
allNothing _ [] = Just []
allNothing f (x : xs)
  | Just _ <- f x = Nothing
  | otherwise = (x :) <$> allNothing f xs

-- scriptRecipientP :: Pl.TypedValidator a -> Prism' PaysScriptConstraint (Pl.DatumType a, Pl.Value)
