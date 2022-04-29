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

module Cooked.MockChain.Attack where

import Cooked.Currencies
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Maybe
import qualified Ledger as Pl hiding (lookup)
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified PlutusTx.AssocMap as Pl
import qualified PlutusTx.IsData as Pl
import qualified PlutusTx.Numeric as Pl (negate)
import qualified PlutusTx.TH as Pl
import Type.Reflection

-- * The type of attacks, and functions to turn optics into attacks

type Attack = TxSkel -> Maybe TxSkel

-- | The simplest way to make an attack from an optic: Apply some function to
-- all of its foci, of fail returning @Nothing@ if nothing is in focus.
mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> a) -> Attack
mkAttack = failover

-- * Some optics on 'TxSkel's, 'Constraint's, etc.

-- There's a recurring pattern here to work around the existential type
-- variables in the constructors in 'Cooked.Tx.Constraints.Type': Define a type
-- that corresponds to the image of the constructor an then use that type as the
-- "codomain" of the optics.

-- ** Picking apart 'TxSkel's

data TxLabel where
  TxLabel :: Show x => Maybe x -> TxLabel

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
    (Pl.ToData a, Show a) =>
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
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a), Typeable a) =>
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

-- scriptRecipientL :: Lens' PaysScriptConstraint

-- data PaysPKWithDatumConstraint where
--   PaysPKWithDatumConstraint ::
--     (Pl.ToData a, Show a) =>
--     Pl.PubKeyHash ->
--     Maybe Pl.StakePubKeyHash ->
--     Maybe a ->
--     Pl.Value ->
--     PaysPKWithDatumConstraint

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

txInValue :: TxSkel -> Pl.Value
txInValue = foldOf (miscConstraintsL % traversed % valueAT)

txOutValue :: TxSkel -> Pl.Value
txOutValue = foldOf (outConstraintsL % traversed % valueL)

-- * Token duplication attack

-- | A token duplication attack which modifies every 'Mints'-constraint of a
-- 'TxSkel' that satisfies some conditions.
dupTokenAttack ::
  Show x =>
  -- | An optional label to attach to the modified 'TxSkel'. It is added using 'addLabel'.
  Maybe x ->
  -- | A function to associate 'Pl.AssetClass'es to functions describing how the
  -- amount of minted tokens of that asset class should be changed by the
  -- attack. The given function @f@ is expected to satisfy
  -- > f ac i == Just j -> i <= j
  -- for all @ac@ and @i@ (i.e. it should never decrease the minted amount). If
  -- @f ac i == Nothing@, the value will be left unchanged.
  (Pl.AssetClass -> Integer -> Maybe Integer) ->
  -- | The wallet of the attacker. Any extra tokens that were minted by the
  -- transaction are paid to this wallet.
  Wallet ->
  Attack
dupTokenAttack lab change attacker skel =
  addLabel lab
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
        surplus = txInValue s <> Pl.negate (txOutValue s)

-- | Maybe add a label to a 'TxSkel'. If there is already a label, the given
-- label will be added, forming a pair @(newlabel, oldlabel)@.
addLabel :: Show x => Maybe x -> TxSkel -> TxSkel
addLabel lab
  | Just newlabel <- lab =
    over
      txLabelL
      ( \case
          TxLabel Nothing -> TxLabel $ Just newlabel
          TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
      )
  | otherwise = id

-- * Datum hijacking attack

(~*~?) :: forall ty a1 a2. (Typeable a1, Typeable a2) => ty a1 -> ty a2 -> Maybe (a1 :~~: a2)
_ ~*~? _ = typeRep @a1 `eqTypeRep` typeRep @a2

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. In that regard, if one wants to be pedantic, this attack is not a "true"
-- datum hijacking attack, but an attack that tests for careless uses of
-- something like 'txInfoOutputs' in places where something like
-- 'getContinuingOutputs' should be used. If it goes through, however, a proper
-- datum hijacking attack will also work, since the translation 'toBuiltinData'
-- does not care whether we use isomorphic or identical datum types (which is
-- the point of datum hijacking).
datumHijackingAttack ::
  forall x a.
  ( Show x,
    Typeable a,
    Pl.UnsafeFromData (Pl.DatumType a),
    Pl.UnsafeFromData (Pl.RedeemerType a)
  ) =>
  -- | An optional label to attach to the modified 'TxSkel'. It is added using 'addLabel'.
  Maybe x ->
  -- | The validator script to steal from.
  Pl.TypedValidator a ->
  -- | A function indicating whether to try the attack on a given datum.
  (Pl.DatumType a -> Bool) ->
  Attack
datumHijackingAttack lab honest change skel =
  addLabel lab
    <$> mkAttack paysScriptConstraintsT changeRecipient skel
  where
    changeRecipient :: PaysScriptConstraint -> PaysScriptConstraint
    changeRecipient c@(PaysScriptConstraint val dat money) =
      case val ~*~? honest of
        Just HRefl ->
          if change dat
            then PaysScriptConstraint thief dat money
            else c
        Nothing -> c
      where
        -- The thief is the validator that alsways returns @True@; we don't need
        -- it to do anything meaningful since we only want to test the
        -- possibility of the attack.
        thief :: Pl.TypedValidator a
        thief =
          Pl.mkTypedValidator @a
            $$(Pl.compile [||\_ _ _ -> True||])
            $$(Pl.compile [||wrap||])
          where
            wrap = Pl.wrapValidator @(Pl.DatumType a) @(Pl.RedeemerType a)

-- Just so we have something to sell in our auction that's not Ada:
-- Have a banana.

bananaAssetClass :: Pl.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Pl.Value
banana = Pl.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Pl.Value -> Integer
bananasIn v = Pl.assetClassValueOf v bananaAssetClass

testSkel :: TxSkel
testSkel = txSkel [Mints (Just ()) [] (banana 5)]
