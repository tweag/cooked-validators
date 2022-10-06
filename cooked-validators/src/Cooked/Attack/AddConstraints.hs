{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Some attacks that add constraints to or remove constraints from
-- transactions.
module Cooked.Attack.AddConstraints where

import Cooked.Attack.Common
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.List
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified PlutusTx.Prelude as Pl

-- | Add constraints to a transaction. This attack uses
-- 'addMiscConstraintsAttack' to add 'MiscConstraint's in a conflict-avoiding
-- and minimal manner, so read also the comment at that function.
--
-- 'OutConstraint's are always added at the end of the list of output
-- constraints, because some contracts rely on the ordering of the (intial
-- segment of) the list of transaction outputs. If that's not to your need, you
-- might be interested in 'permutOutAttack'.
--
-- The returned 'Constraints' will be the constraints that were *actually* added
-- to the transaction, which might differ from your specified constraints (but
-- only in 'MiscConstraint's, so before the arrow ':=>:'). In every case, this
-- attack ensures that every constraint you specified will be included in the
-- constraints of the modified transaction.
addConstraintsAttack :: Constraints -> Attack Constraints
addConstraintsAttack (is :=>: os) = do
  addedMiscConstraints <- mapM addMiscConstraintAttack is
  overAttack outConstraintsL (++ os) -- appended at the end
  return (catMaybes addedMiscConstraints :=>: os)

-- * Adding and removing 'MiscConstraint's from transactions

-- | This attack adds 'MiscConstraint's to a transaction, in a way that avoids
-- conflicts and also potentially simplifies the constraints. In particular:
--
-- - Adding a 'SpendsPK' for an UTxO which is already consumed by the unmodified
--   transaction will leave the transaction unmodified.
--
-- - Adding a 'SpendsScript' for an UTxO that is already consumed by the
--   unmodified transaction will lead to failure, unless the 'SpendsScript'
--   constraint on the unmodified transaction uses exactly the same validator,
--   redeemer, and datum. In that case, the attack will leave the transaction
--   unmodified.
--
-- - 'Mints' constraints are added without any additional checks.
--
-- - if any constraint concerning time (i.e. 'Before', 'After', or 'ValidateIn')
--   is added, the resulting transaction will only have one 'ValidateIn'
--   constraint, specifying as the validity time range the intersection of all
--   the pre-existing time ranges on the transaction and the new time range.
--
-- - if a 'SignedBy' constraint is added, the resulting transaction will
--   similarly have only one 'SignedBy' constraint, which holds all of the
--   signers required by the unmodified transaction, together with the new
--   signers.
--
-- The returned 'MiscConstraint' is either @Just@ the constraint that was added
-- to the transaction, or @Nothing@, if your constraint was already present on
-- the unmodified transaction.
addMiscConstraintAttack :: MiscConstraint -> Attack (Maybe MiscConstraint)
addMiscConstraintAttack (SpendsScript v r (o, d)) = addSpendsScriptAttack v r (o, d)
addMiscConstraintAttack (SpendsPK o) = addSpendsPKAttack o
addMiscConstraintAttack (Mints r ps x) = addMintsAttack r ps x
addMiscConstraintAttack (Before b) =
  let leftUnbounded = Pl.to b
   in addValidateInAttack leftUnbounded
addMiscConstraintAttack (After a) =
  let rightUnbounded = Pl.from a
   in addValidateInAttack rightUnbounded
addMiscConstraintAttack (ValidateIn range) = addValidateInAttack range
addMiscConstraintAttack (SignedBy s) = addSignedByAttack s

-- | This attack removes some 'MiscConstraint's from a transaction. Exactly
-- those constraints that return @Just@ something are removed from the
-- transaction.
--
-- The attack returns a list of all the @Just@ values that were returned by the
-- removed constraints.
removeMiscConstraintsAttack :: (MiscConstraint -> Maybe a) -> Attack [a]
removeMiscConstraintsAttack removePred = do
  mcs <- viewAttack miscConstraintsL
  let (removed, kept) = partitionMaybe removePred mcs
  setAttack miscConstraintsL kept
  return removed

-- | Partition a list into two lists, where the first output list contains all
-- values that were returned by @Just@, and the second records all elements of
-- the input list that returned @Nothing@. The order of elements is preserved in
-- both output lists.
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe p l = accumulate l [] []
  where
    accumulate [] j n = (reverse j, reverse n)
    accumulate (x : xs) j n = case p x of
      Just y -> accumulate xs (y : j) n
      Nothing -> accumulate xs j (x : n)

-- | this attack ensures that a certain 'SpendsScript' constraint is present on
-- a transaction.
--
-- Adding a 'SpendsScript' for an UTxO that is already consumed by the
-- unmodified transaction will lead to failure, unless the 'SpendsScript'
-- constraint on the unmodified transaction uses exactly the same validator,
-- redeemer, and datum. In that case, the attack will leave the transaction
-- unmodified.
--
-- This attack returns @Just@ the added constraint iff the transaction was
-- modified.
addSpendsScriptAttack ::
  forall a.
  SpendsConstrs a =>
  L.TypedValidator a ->
  L.RedeemerType a ->
  (SpendableOut, L.DatumType a) ->
  Attack (Maybe MiscConstraint)
addSpendsScriptAttack v r (o, d) = do
  present <- viewAttack (partsOf $ spendsScriptConstraintsT % spendsScriptConstraintTypeP @a)
  let clashing = filter (\(_, _, (o', _)) -> o == o') present
  case clashing of
    [] ->
      let newConstraint = SpendsScript v r (o, d)
       in do
            overAttack miscConstraintsL (newConstraint :)
            return $ Just newConstraint
    [(v', r', (_, d'))] ->
      if v' == v && r' Pl.== r && d' Pl.== d
        then return Nothing
        else failingAttack
    _ -> return Nothing -- Something's already wrong with the unmodified
    -- transaction, since it spends the same UTxO at least
    -- twice. Let's not fail nonetheless, maybe there's a
    -- reason for the madness.

-- | This attack ensures that a certain 'SpendsPK' constraint is present on a
-- transaction.
--
-- This attack returns @Just@ the added constraint iff the transaction was
-- modified.
addSpendsPKAttack :: SpendableOut -> Attack (Maybe MiscConstraint)
addSpendsPKAttack extraUtxo = do
  consumed <- viewAttack (partsOf $ miscConstraintT % spendsPKConstraintP)
  if extraUtxo `elem` consumed
    then return Nothing
    else
      let newConstraint = SpendsPK extraUtxo
       in do
            overAttack miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- | This attack adds a 'Mints' constraint.
--
-- This attack always returns @Just@ the added constraint. Its return type is in
-- 'Maybe' in order to be homogeneous with 'addSpendsScriptAttack',
-- 'addSpendsPKAttack', 'addValidateInAttack', and 'addSignedByAttack'.
addMintsAttack ::
  MintsConstrs a =>
  Maybe a ->
  [L.MintingPolicy] ->
  L.Value ->
  Attack (Maybe MiscConstraint)
addMintsAttack r ps x =
  let newConstraint = Mints r ps x
   in do
        overAttack miscConstraintsL (newConstraint :)
        return $ Just newConstraint

-- | This attack removes all time constraints (i.e. 'Before', 'After', and
-- 'ValidateIn') from a transaction.
--
-- Returns the validity time range of the unmodified transaction.
removeTimeConstraintsAttack :: Attack L.POSIXTimeRange
removeTimeConstraintsAttack = do
  timeRanges <- removeMiscConstraintsAttack toTimeRange
  return $ foldr Pl.intersection Pl.always timeRanges
  where
    toTimeRange = \case
      Before b -> Just $ Pl.to b
      After a -> Just $ Pl.from a
      ValidateIn i -> Just i
      _ -> Nothing

-- | This attack restricts the validity time range of a transaction to the
-- intersection of the given range and its current validity time range.
--
-- If this attack performs any change at all, it also "cleans up" the time
-- constraints (i.e. 'Before', 'After', and 'ValidateIn'), so that the resulting
-- transaction has only one 'ValidateIn' constraint.
--
-- This attack returns @Just@ the added constraint iff the transaction was
-- modified.
addValidateInAttack :: L.POSIXTimeRange -> Attack (Maybe MiscConstraint)
addValidateInAttack range = do
  unmodifiedSkel <- saveHereAttack
  oldRange <- removeTimeConstraintsAttack
  let newRange = range `Pl.intersection` oldRange
  if newRange == oldRange
    then do
      restoreHereAttack unmodifiedSkel
      return Nothing
    else
      let newConstraint = ValidateIn newRange
       in do
            overAttack miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- | This attack removes all 'SignedBy' constraints from a transaction. It
-- returns a list of the signers it removed.
removeSignedByAttack :: Attack [L.PubKeyHash]
removeSignedByAttack = do
  signers <- removeMiscConstraintsAttack (\case SignedBy s -> Just s; _ -> Nothing)
  return $ concat signers

-- | This attack adds signers to a transaction. It also combines all 'SignedBy'
-- constraints of the transaction into one, if it performs any change at all.
--
-- This attack returns @Just@ the added constraint iff the transaction was
-- modified.
addSignedByAttack :: [L.PubKeyHash] -> Attack (Maybe MiscConstraint)
addSignedByAttack signers = do
  unmodifiedSkel <- saveHereAttack
  oldSigners <- removeSignedByAttack
  let newSigners = signers \\ oldSigners
  if null newSigners
    then do
      restoreHereAttack unmodifiedSkel
      return Nothing
    else
      let newConstraint = SignedBy $ newSigners ++ oldSigners
       in do
            overAttack miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- * Adding and Removing 'OutConstraint's

-- | Add an 'OutConstraint' to a transaction. The additional constraint will be
-- added to the end of the list of output constraints. This is because some
-- contracts rely on the shape of (the initial segment of) the list of
-- transaction outputs. Try 'permutOutAttack' if you want to change the ordering
-- of output constraints.
addOutConstraintAttack :: OutConstraint -> Attack ()
addOutConstraintAttack oc = overAttack outConstraintsL (++ [oc])

-- | This attack removes some 'OutConstraint's from a transaction. Exactly those
-- constraints that return @Just@ something are removed from the transaction.
--
-- The attack returns a list of all the @Just@ values that were returned by the
-- removed constraints.
removeOutConstraintsAttack :: (OutConstraint -> Maybe a) -> Attack [a]
removeOutConstraintsAttack removePred = do
  mcs <- viewAttack outConstraintsL
  let (removed, kept) = partitionMaybe removePred mcs
  setAttack outConstraintsL kept
  return removed
