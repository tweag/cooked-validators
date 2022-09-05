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
-- to the transaction (which might be different from the one you specified in
-- case of time constraints, in which case the returned constraint holds the new
-- validity time range of the transaction), or @Nothing@, if your constraint was
-- already present on the unmodiified transaction.
addMiscConstraintAttack :: MiscConstraint -> Attack (Maybe MiscConstraint)
addMiscConstraintAttack (SpendsScript v r (o, d)) =
  do
    added <- addSpendsScriptAttack v r (o, d)
    if added
      then return $ Just $ SpendsScript v r (o, d)
      else return Nothing
addMiscConstraintAttack (SpendsPK o) =
  do
    added <- addSpendsPKAttack o
    if added
      then return $ Just $ SpendsPK o
      else return Nothing
addMiscConstraintAttack (Mints r ps x) =
  addMintsAttack r ps x
    >> return (Just $ Mints r ps x)
addMiscConstraintAttack (Before b) =
  let leftUnbounded = Pl.to b -- is this correct, i.e. should the end time be excluded?
   in addValidateInAttack leftUnbounded
        >>= return . Just . ValidateIn . Pl.intersection leftUnbounded
addMiscConstraintAttack (After a) =
  let rightUnbounded = Pl.from a -- is this correct, i.e. should the start time be excluded?
   in addValidateInAttack rightUnbounded
        >>= return . Just . ValidateIn . Pl.intersection rightUnbounded
addMiscConstraintAttack (ValidateIn range) =
  addValidateInAttack range
    >>= return . Just . ValidateIn . Pl.intersection range
addMiscConstraintAttack (SignedBy s) = addSignedByAttack s >>= return . Just . SignedBy

-- | This attack removes some 'MiscConstraint's from a transaction. It returns a
-- list of the removed constraints.
removeMiscConstraintsAttack :: (MiscConstraint -> Bool) -> Attack [MiscConstraint]
removeMiscConstraintsAttack removePred = do
  mcs <- viewAttack miscConstraintsL
  let (removed, kept) = partition removePred mcs
  setAttack miscConstraintsL kept
  return removed

-- | This attack ensures that a certain 'SpendsScript' constraint is present on
-- a transaction.
--
-- Adding a 'SpendsScript' for an UTxO that is already consumed by the
-- unmodified transaction will lead to failure, unless the 'SpendsScript'
-- constraint on the unmodified transaction uses exactly the same validator,
-- redeemer, and datum. In that case, the attack will leave the transaction
-- unmodified.
--
-- This attack returns @True@ iff the transaction was modified.
addSpendsScriptAttack ::
  forall a.
  SpendsConstrs a =>
  L.TypedValidator a ->
  L.RedeemerType a ->
  (SpendableOut, L.DatumType a) ->
  Attack Bool
addSpendsScriptAttack v r (o, d) = do
  present <- viewAttack (partsOf $ spendsScriptConstraintsT % spendsScriptConstraintTypeP @a)
  let clashing = filter (\(_, _, (o', _)) -> o == o') present
  case clashing of
    [] -> do
      overAttack miscConstraintsL (SpendsScript v r (o, d) :)
      return True
    [(v', r', (_, d'))] ->
      if v' == v && r' Pl.== r && d' Pl.== d
        then return False
        else failingAttack
    _ -> return False -- Something's already wrong with the unmodified
    -- transaction, since it spends the same UTxO at least twice. Let's not fail
    -- nonetheless, maybe there's a reason for the madness.

-- | This attack ensures that a certain 'SpendsPK' constraint is present on a
-- transaction.
--
-- Returns @True@ iff the transaction was changed. (In particular, this attack
-- never fails.)
addSpendsPKAttack :: SpendableOut -> Attack Bool
addSpendsPKAttack extraUtxo = do
  consumed <- viewAttack (partsOf $ miscConstraintT % spendsPKConstraintP)
  if extraUtxo `elem` consumed
    then return False
    else do
      overAttack miscConstraintsL (SpendsPK extraUtxo :)
      return True

-- | This attack adds a 'Mints' constraint.
addMintsAttack ::
  MintsConstrs a =>
  Maybe a ->
  [L.MintingPolicy] ->
  L.Value ->
  Attack ()
addMintsAttack r ps x = overAttack miscConstraintsL (Mints r ps x :)

-- | This attack removes all time constraints (i.e. 'Before', 'After', and
-- 'ValidateIn') from a transaction.
--
-- Returns the validity time range of the unmodified transaction.
removeTimeConstraintsAttack :: Attack L.POSIXTimeRange
removeTimeConstraintsAttack = do
  timeConstraints <-
    removeMiscConstraintsAttack
      ( \case
          Before _ -> True
          After _ -> True
          ValidateIn _ -> True
          _ -> False
      )
  return $
    foldr
      ( \case
          -- I don't know if 'Before' and 'After' should include the endpoint or
          -- not. Since the current implementation of the 'Before' and 'After'
          -- constraints includes the endpoints, I will do so here as well.
          Before b -> Pl.intersection (Pl.to b)
          After a -> Pl.intersection (Pl.from a)
          ValidateIn i -> Pl.intersection i
          _ -> id
      )
      Pl.always
      timeConstraints

-- | This attack restricts the validity time range of a transaction to the
-- intersection of the given range and its current validity time range.
--
-- It also "cleans up" the time constraints (i.e. 'Before', 'After', and
-- 'ValidateIn'), so that the resulting transaction has only one 'ValidateIn'
-- constraint.
--
-- Returns the validity time range of the unmodified transaction.
addValidateInAttack :: L.POSIXTimeRange -> Attack L.POSIXTimeRange
addValidateInAttack range = do
  oldRange <- removeTimeConstraintsAttack
  overAttack miscConstraintsL (ValidateIn (range `Pl.intersection` oldRange) :)
  return oldRange

-- | This attack removes all 'SignedBy' constraints from a transaction. It
-- returns a list of the signers it removed.
removeSignedByAttack :: Attack [L.PubKeyHash]
removeSignedByAttack = do
  removed <- removeMiscConstraintsAttack (\case SignedBy _ -> True; _ -> False)
  return $ concatMap (\case SignedBy s -> s; _ -> []) removed

-- | This attack adds signers to a transaction. It also combines all 'SignedBy'
-- constraints of the transaction into one.
--
-- Returns a list of all signers that were added (i.e. that were not already
-- present on the unmodified transaction).
addSignedByAttack :: [L.PubKeyHash] -> Attack [L.PubKeyHash]
addSignedByAttack signers = do
  oldSigners <- removeSignedByAttack
  let newSigners = filter (`notElem` oldSigners) signers
  overAttack miscConstraintsL (SignedBy (newSigners ++ oldSigners) :)
  return newSigners

-- * Adding and Removing 'OutConstraint's

-- | Add an 'OutConstraint' to a transaction. The additional constraint will be
-- added to the end of the list of output constraints. This is because some
-- contracts rely on the shape of (the initial segment of) the list of
-- transaction outputs. Try 'permutOutAttack' if you want to change the ordering
-- of output constraints.
addOutConstraintAttack :: OutConstraint -> Attack ()
addOutConstraintAttack oc = overAttack outConstraintsL (++ [oc])

-- | Add a 'paysPK' constraint to a transaction. The additional constraint will
-- be added to the end of the list of output constraints.
addPaysPKAttack ::
  L.PubKeyHash ->
  L.Value ->
  Attack ()
addPaysPKAttack h v = addOutConstraintAttack $ paysPK h v

-- | Add a 'PaysScript' to a transaction. The additional constraint will be
-- added to the end of the list of output constraints.
addPaysScriptAttack ::
  PaysScriptConstrs a =>
  L.TypedValidator a ->
  L.DatumType a ->
  L.Value ->
  Attack ()
addPaysScriptAttack v d x = addOutConstraintAttack $ PaysScript v d x

-- | This attack removes some 'OutConstraint's from a transaction. It returns a
-- list of the removed constraints.
removeOutConstraintsAttack :: (OutConstraint -> Bool) -> Attack [OutConstraint]
removeOutConstraintsAttack removePred = do
  mcs <- viewAttack outConstraintsL
  let (removed, kept) = partition removePred mcs
  setAttack outConstraintsL kept
  return removed
