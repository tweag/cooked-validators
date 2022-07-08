{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cooked.Attack where

import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.RawUPLC (unsafeTypedValidatorFromUPLC)
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Data.Map as M
import Data.Maybe
import qualified Ledger as L hiding (validatorHash)
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl (negate)
import Type.Reflection

-- The idea of this module: Turning optics into attacks
-------------------------------------------------------
--
-- In cooked-validators, a _single transaction attack_ on a smart contract is a
-- function that modifies one transaction: An attacker applies this function to
-- a transaction in an otherwise normal chain of transactions, somehow fools the
-- validator script(s), and profits. So, attacks in cooked-valiadators should
-- modify 'TxSkel's.
--
-- It would be nice to have a collection of parametric attacks, together with an
-- easy way to add new attacks to that collection. This module contains the
-- beginnings of such a collection, and also a hopefully useful mechanism to
-- extend it.
--
-- Since most attacks are of the form "look deep into the nested data types
-- within a 'TxSkel' and change something", The idea is to use the optics
-- defined in "Cooked.Tx.Constraints.Optics" and turn them into suitable
-- modifications.
--
-- Optic attack example: Token duplication
------------------------------------------
--
-- There's a 'dupTokenAttack' below, and it's implementation is quite readable;
-- the purpose of the following is not to explain that function in detail, but
-- to illustrate how one should write new attacks using this module.
--
-- A _token duplication_ attack consists in trying to increase the amount of
-- tokens a transaction mints and paying the surplus to an attacker.  An idea to
-- implement the token duplication attack would then be
--
-- > naiveDupTokenAttack :: Wallet -> (Value -> Value) -> TxSkel -> TxSkel
-- > naiveDupTokenAttack attacker increaseValue =
-- >   paySurplusTo attacker . over (mintsConstraintT % valueL) increaseValue
--
-- where @paySurplusTo :: Wallet -> TxSkel -> TxSkel@ is a suitable function
-- that modifies a 'TxSkel' to play any extra minted tokens to a given
-- wallet.
--
-- This is idea is almost right, save for the type of @naiveDupTokenAttack@: If
-- @increaseValue@ did not in fact _change_ any of the minted values values, or
-- if there were no 'MintsConstraint's in the transaction under consideration,
-- we have no way to detect this failure. That's why this module provides
-- functions like
--
-- > mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> Maybe a) -> TxSkel -> Maybe TxSkel
--
-- which is a kind of "'over' with failure": Given any @optic@ and @f@, we have
-- @mkAttack optic f skel == Just skel'@ if and only if
--
-- - @optic@ has at least one focus on @skel@, and
--
-- - @f@ returns @Just@ on at least one of the foci of @optic@ on @skel@.
--
-- In that case, @skel'@ is the transaction skeleton we get by replacing all
-- foci @x@ with @f x == Just x'@ with @x'@. This allows us to write
-- 'dupTokenAttack' as below.
--
-- Note that this 'dupTokenAttack' is contract-agnostic. Also note that, despite
-- (because?) of its generality, it is very easy to implement. Our growing
-- collection of optics will hopefully mean that it will become easier and
-- easier to write attacks.
--
-- Using attacks
----------------
--
-- The attacks from this module can be used with the modailties in any
-- 'MonadModalMockChain'. These allow combintations of single-transaction
-- attacks into coordinated attacks of many transactions.

-- * The type of attacks, and functions to turn optics into attacks

-- | The type of attacks that operate on a single transaction. The idea is to
-- try to modify a transaction, or return @Nothing@ if the modification does not
-- apply; use in a 'MonadModalMockChain'.
type Attack = MockChainSt -> TxSkel -> Maybe TxSkel

-- | The simplest way to make an attack from an optic: Try to apply a given
-- function of type @a -> Maybe a@ to all foci of an optic, modifying all foci
-- that return @Just@. This attack returns @Just@ if at least one modification
-- was successful, i.e. if at least one of the foci evaluates to @Just@;
-- otherwise it returns @Nothing@.
mkAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (a -> Maybe a) ->
  Attack
mkAttack optic f =
  mkAccumLAttack
    optic
    ( \_ flag x -> case f x of
        Just y -> (y, True)
        Nothing -> (x, flag)
    )
    False
    (\_ skel flag -> if flag then Just skel else Nothing)

-- | A more fine-grained attack: Traverse all foci of the optic from the left to
-- the right, while counting the foci to which the modification successfully
-- applies, and modify only those modifiable foci whose index satisfies a given
-- predicate. This is useful if you have many indentical outputs but you only
-- want to modify a few of them.
mkSelectAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (a -> Maybe a) ->
  -- | Maybe the modification applies to (i.e. returns @Just@ on) more than one
  -- focus. Use this function to select the foci to modify by the order in which
  -- they are traversed. If you set this function to @const True@, you should
  -- probably use 'mkAttack', because that the semantic of that function.
  (Integer -> Bool) ->
  Attack
mkSelectAttack optic f select =
  mkAccumLAttack
    optic
    ( \_ (flag, index) x -> case f x of
        Just y ->
          if select index
            then (y, (True, index + 1))
            else (x, (flag, index + 1))
        Nothing -> (x, (flag, index))
    )
    (False, 0)
    (\_ skel (flag, _) -> if flag then Just skel else Nothing)

-- | A very general attack: Traverse all foci of the optic from the left to the
-- right, collecting an accumulator while also (optionally) modifying the
-- 'TxSkel'. At the end, look at the modified 'TxSkel' and the accumulator to
-- decide whether the traversal was a success, or whether we should return
-- @Nothing@.
mkAccumLAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | function that describes the modification of the accumulator and the
  -- current focus.
  (MockChainSt -> acc -> a -> (a, acc)) ->
  -- | initial accumulator
  acc ->
  -- | function to decide whether the traversal modified the 'TxSkel' in the
  -- desired way. This will typically look like this:
  -- > \state skel acc -> if someTest state skel acc
  -- >                    then Just (someFinalModification state skel acc)
  -- >                    else Nothing
  (MockChainSt -> TxSkel -> acc -> Maybe TxSkel) ->
  Attack
mkAccumLAttack optic f initAcc test mcst skel =
  let (skel', acc) = mapAccumLOf optic (f mcst) initAcc skel
   in test mcst skel' acc

-- * General helpers

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

-- * Token duplication attack

-- | A token duplication attack which modifies every 'Mints'-constraint of a
-- 'TxSkel' that satisfies some conditions. This adds a 'DupTokenLbl' to the
-- labels of the transaction using 'addLabel'.
dupTokenAttack ::
  -- | A function describing how the amount of minted tokens of an asset class
  -- should be changed by the attack. The given function @f@ should probably satisfy
  -- > f ac i == Just j -> i < j
  -- for all @ac@ and @i@, i.e. it should increase in the minted amount.
  -- If it does *not* increase the minted amount, or if @f ac i == Nothing@, the
  -- minted amount will be left unchanged.
  (L.AssetClass -> Integer -> Maybe Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  Attack
dupTokenAttack change attacker mcst skel =
  addLabel DupTokenLbl . paySurplusTo attacker skel
    <$> mkAttack (mintsConstraintsT % valueL) increaseValue mcst skel
  where
    increaseValue :: L.Value -> Maybe L.Value
    increaseValue v =
      case someJust
        ( \(ac, i) -> case change ac i of
            Nothing -> Nothing
            Just j -> if i < j then Just (ac, j) else Nothing
        )
        (view flattenValueI v) of
        Just l -> Just $ review flattenValueI l
        Nothing -> Nothing

    paySurplusTo :: Wallet -> TxSkel -> TxSkel -> TxSkel
    paySurplusTo w skelOld skelNew = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) skelNew
      where
        txSkelMintValue s = foldOf (mintsConstraintsT % valueL) s
        surplus = txSkelMintValue skelNew <> Pl.negate (txSkelMintValue skelOld)

    -- Try to use the given function to modify the elements of the given list. If
    -- at least one modification is successful, return 'Just' the modified list,
    -- otherwise fail returning 'Nothing'.
    someJust :: (a -> Maybe a) -> [a] -> Maybe [a]
    someJust _ [] = Nothing
    someJust f (x : xs) = case f x of
      Just y -> Just $ y : map (\a -> fromMaybe a (f a)) xs
      Nothing -> (x :) <$> someJust f xs

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show)

-- * Datum hijacking attack

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that the (relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work. A
-- 'DatumHijackingLbl' with the hash of the "thief" validator is added to the
-- labels of the 'TxSkel' using 'addLabel'.
datumHijackingAttack ::
  forall a.
  ( Typeable a,
    Pl.UnsafeFromData (L.DatumType a),
    Pl.UnsafeFromData (L.RedeemerType a)
  ) =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Bool) ->
  -- | If the selection predicate matches more than one output, select the n-th
  -- output(s) with this predicate.
  (Integer -> Bool) ->
  Attack
datumHijackingAttack change select mcst skel =
  let thief = datumHijackingTarget @a

      changeRecipient :: PaysScriptConstraint -> Maybe PaysScriptConstraint
      changeRecipient (PaysScriptConstraint val dat money) =
        -- checks whether val _is of the same type as_ the thief, they're obviously different scripts.
        case val ~*~? thief of
          Just HRefl ->
            if change val dat money
              then Just $ PaysScriptConstraint thief dat money
              else Nothing
          Nothing -> Nothing
   in addLabel (DatumHijackingLbl $ L.validatorHash thief)
        <$> mkSelectAttack paysScriptConstraintsT changeRecipient select mcst skel

newtype DatumHijackingLbl = DatumHijackingLbl L.ValidatorHash
  deriving (Show, Eq)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: L.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()

-- * Double satisfaction attack

inFibre :: forall a ty b. (Typeable a, Typeable b) => ty b -> Maybe (a :~~: b)
inFibre _ = typeRep @a `eqTypeRep` typeRep @b

spendsScriptConstraintsT :: Traversal' TxSkel SpendsScriptConstraint
spendsScriptConstraintsT = miscConstraintsL % traversed % spendsScriptConstraintP

data DoubleSatParams b = DoubleSatParams
  { dsExtraInputOwner :: L.TypedValidator b,
    dsExtraInputPred :: L.Value -> L.DatumType b -> Bool,
    dsExtraInputRedeemer :: L.RedeemerType b,
    dsSelectSpendsScript :: SpendsScriptConstraint -> Bool,
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
  addLabel DoubleSatLbl
    . paySurplusTo dsAttacker
    <$> mkAccumLAttack
      (singular spendsScriptConstraintsT)
      ( \mcst _ constr ->
          if dsSelectSpendsScript constr
            then (constr, SpendsScript dsExtraInputOwner dsExtraInputRedeemer <$> extraUtxo)
            else (constr, Nothing)
      )
      Nothing
      ( \mcst sk mConstr -> case mConstr of
          Nothing -> Nothing
          Just c -> Just $ over miscConstraintsL (c :) sk
      )
      mcst
      skel
  where
    getDatumFromHash :: L.DatumHash -> Maybe (L.DatumType b)
    getDatumFromHash dh = do
      L.Datum d <- M.lookup dh (mcstDatums mcst)
      Pl.fromBuiltinData d

    -- try to get a UTxO from the 'MockChainSt' that belongs to
    -- 'dsExtraInputOwner' and whose value and datum satisfy 'dsExtraInputPred'
    extraUtxo :: Maybe (SpendableOut, L.DatumType b)
    extraUtxo =
      let -- a list of all UTxOs belonging to 'dsExtraInputOwner' and satisfying
          -- the 'dsExtraInputPred'
          candidates =
            M.toList $
              M.filter
                ( \case
                    L.TxOut a v (Just dh) ->
                      (a == L.validatorAddress dsExtraInputOwner)
                        && maybe False (dsExtraInputPred v) (getDatumFromHash dh)
                    _ -> False
                )
                (L.getIndex $ mcstIndex mcst)
       in case candidates of
            (oref, L.TxOut a v (Just dh)) : _ ->
              (( oref,
                 L.ScriptChainIndexTxOut
                   a
                   (Left $ L.validatorHash dsExtraInputOwner)
                   (Left dh)
                   v
               ),)
                <$> getDatumFromHash dh
            _ -> Nothing

    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w sk = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) sk
      where
        surplus = txSkelInValue skel <> Pl.negate (txSkelOutValue skel)

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
