{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Some attacks that add constraints to or remove constraints from
-- transactions.
module Cooked.Attack.Tweak.AddConstraints where

import Cooked.Attack.Tweak.Common
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.List
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified PlutusTx.Prelude as Pl

-- | Add constraints to a transaction. This tweak uses
-- 'addMiscConstraintsTweak' to add 'MiscConstraint's in a conflict-avoiding
-- and minimal manner, so read also the comment at that function.
--
-- If there are contradictions between the constraints that are already present
-- and the additional constraints, this tweak fails.
--
-- 'OutConstraint's are always added at the end of the list of output
-- constraints, because some contracts rely on the ordering of the (intial
-- segment of) the list of transaction outputs. If that's not to your need, you
-- might be interested in 'permutOutTweak'.
--
-- The returned 'Constraints' will be the constraints that were *actually* added
-- to the transaction, which might differ from your specified constraints (but
-- only in 'MiscConstraint's, so on the left-hand side of ':=>:'). In every
-- case, this tweak ensures that every constraint you specified will be
-- included in the constraints of the modified transaction.
addConstraintsTweak :: Constraints -> Tweak Constraints
addConstraintsTweak (is :=>: os) = do
  addedMiscConstraints <- mapM addMiscConstraintTweak is
  overTweak outConstraintsL (++ os) -- appended at the end
  return (catMaybes addedMiscConstraints :=>: os)

-- * Adding and removing 'MiscConstraint's from transactions

-- | This tweak adds 'MiscConstraint's to a transaction, in a way that avoids
-- conflicts and also potentially simplifies the constraints. In particular:
--
-- - Adding a 'SpendsPK' for an UTxO which is already consumed by the unmodified
--   transaction will leave the transaction unmodified.
--
-- - Adding a 'SpendsScript' for an UTxO that is already consumed by the
--   unmodified transaction will lead to failure, unless the 'SpendsScript'
--   constraint on the unmodified transaction uses exactly the same validator,
--   redeemer, and datum. In that case, the tweak will leave the transaction
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
addMiscConstraintTweak :: MiscConstraint -> Tweak (Maybe MiscConstraint)
addMiscConstraintTweak (SpendsScript v r o) = addSpendsScriptTweak v r o
addMiscConstraintTweak (SpendsPK o) = addSpendsPKTweak o
addMiscConstraintTweak (Mints r ps x) = addMintsTweak r ps x
addMiscConstraintTweak (Before b) =
  let leftUnbounded = Pl.to b
   in addValidateInTweak leftUnbounded
addMiscConstraintTweak (After a) =
  let rightUnbounded = Pl.from a
   in addValidateInTweak rightUnbounded
addMiscConstraintTweak (ValidateIn range) = addValidateInTweak range
addMiscConstraintTweak (SignedBy s) = addSignedByTweak s

-- | This tweak removes some 'MiscConstraint's from a transaction. Exactly
-- those constraints that return @Just@ something are removed from the
-- transaction.
--
-- The tweak returns a list of all the @Just@ values that were returned by the
-- removed constraints.
removeMiscConstraintsTweak :: (MiscConstraint -> Maybe a) -> Tweak [a]
removeMiscConstraintsTweak removePred = do
  mcs <- viewTweak miscConstraintsL
  let (removed, kept) = partitionMaybe removePred mcs
  setTweak miscConstraintsL kept
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

-- | this tweak ensures that a certain 'SpendsScript' constraint is present on
-- a transaction.
--
-- Adding a 'SpendsScript' for an UTxO that is already consumed by the
-- unmodified transaction will lead to failure, unless the 'SpendsScript'
-- constraint on the unmodified transaction uses exactly the same validator,
-- redeemer, and datum. In that case, the tweak will leave the transaction
-- unmodified.
--
-- This tweak returns @Just@ the added constraint iff the transaction was
-- modified.
addSpendsScriptTweak ::
  forall a.
  SpendsConstrs a =>
  L.TypedValidator a ->
  L.RedeemerType a ->
  SpendableOut ->
  Tweak (Maybe MiscConstraint)
addSpendsScriptTweak v r o = do
  present <- viewTweak (partsOf $ spendsScriptConstraintsT % spendsScriptConstraintTypeP @a)
  let clashing = filter (\(_, _, o') -> o == o') present
  case clashing of
    [] ->
      let newConstraint = SpendsScript v r o
       in do
            overTweak miscConstraintsL (newConstraint :)
            return $ Just newConstraint
    [(v', r', _)] ->
      if v' == v && r' Pl.== r
        then return Nothing
        else failingTweak
    _ -> return Nothing -- Something's already wrong with the unmodified
    -- transaction, since it spends the same UTxO at least
    -- twice. Let's not fail nonetheless, maybe there's a
    -- reason for the madness.

-- | This tweak ensures that a certain 'SpendsPK' constraint is present on a
-- transaction.
--
-- This tweak returns @Just@ the added constraint iff the transaction was
-- modified.
addSpendsPKTweak :: SpendableOut -> Tweak (Maybe MiscConstraint)
addSpendsPKTweak extraUtxo = do
  consumed <- viewTweak (partsOf $ miscConstraintT % spendsPKConstraintP)
  if extraUtxo `elem` consumed
    then return Nothing
    else
      let newConstraint = SpendsPK extraUtxo
       in do
            overTweak miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- | This tweak adds a 'Mints' constraint.
--
-- This tweak always returns @Just@ the added constraint. Its return type is in
-- 'Maybe' in order to be homogeneous with 'addSpendsScriptTweak',
-- 'addSpendsPKTweak', 'addValidateInTweak', and 'addSignedByTweak'.
addMintsTweak ::
  MintsConstrs a =>
  Maybe a ->
  [L.MintingPolicy] ->
  L.Value ->
  Tweak (Maybe MiscConstraint)
addMintsTweak r ps x =
  let newConstraint = Mints r ps x
   in do
        overTweak miscConstraintsL (newConstraint :)
        return $ Just newConstraint

-- | This tweak removes all time constraints (i.e. 'Before', 'After', and
-- 'ValidateIn') from a transaction.
--
-- Returns the validity time range of the unmodified transaction.
removeTimeConstraintsTweak :: Tweak L.POSIXTimeRange
removeTimeConstraintsTweak = do
  timeRanges <- removeMiscConstraintsTweak toTimeRange
  return $ foldr Pl.intersection Pl.always timeRanges
  where
    toTimeRange = \case
      Before b -> Just $ Pl.to b
      After a -> Just $ Pl.from a
      ValidateIn i -> Just i
      _ -> Nothing

-- | This tweak restricts the validity time range of a transaction to the
-- intersection of the given range and its current validity time range.
--
-- If this tweak performs any change at all, it also "cleans up" the time
-- constraints (i.e. 'Before', 'After', and 'ValidateIn'), so that the resulting
-- transaction has only one 'ValidateIn' constraint.
--
-- This tweak returns @Just@ the added constraint iff the transaction was
-- modified.
addValidateInTweak :: L.POSIXTimeRange -> Tweak (Maybe MiscConstraint)
addValidateInTweak range = do
  unmodifiedSkel <- getTxSkel
  oldRange <- removeTimeConstraintsTweak
  let newRange = range `Pl.intersection` oldRange
  if newRange == oldRange
    then do
      setTxSkel unmodifiedSkel
      return Nothing
    else
      let newConstraint = ValidateIn newRange
       in do
            overTweak miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- | This tweak removes all 'SignedBy' constraints from a transaction. It
-- returns a list of the signers it removed.
removeSignedByTweak :: Tweak [L.PubKeyHash]
removeSignedByTweak = do
  signers <- removeMiscConstraintsTweak (\case SignedBy s -> Just s; _ -> Nothing)
  return $ concat signers

-- | This tweak adds signers to a transaction. It also combines all 'SignedBy'
-- constraints of the transaction into one, if it performs any change at all.
--
-- This tweak returns @Just@ the added constraint iff the transaction was
-- modified.
addSignedByTweak :: [L.PubKeyHash] -> Tweak (Maybe MiscConstraint)
addSignedByTweak signers = do
  unmodifiedSkel <- getTxSkel
  oldSigners <- removeSignedByTweak
  let newSigners = signers \\ oldSigners
  if null newSigners
    then do
      setTxSkel unmodifiedSkel
      return Nothing
    else
      let newConstraint = SignedBy $ newSigners ++ oldSigners
       in do
            overTweak miscConstraintsL (newConstraint :)
            return $ Just newConstraint

-- * Adding and Removing 'OutConstraint's

-- | Add an 'OutConstraint' to a transaction. The additional constraint will be
-- added to the end of the list of output constraints. This is because some
-- contracts rely on the shape of (the initial segment of) the list of
-- transaction outputs. Try 'permutOutTweak' if you want to change the ordering
-- of output constraints.
addOutConstraintTweak :: OutConstraint -> Tweak ()
addOutConstraintTweak oc = overTweak outConstraintsL (++ [oc])

-- | This tweak removes some 'OutConstraint's from a transaction. Exactly those
-- constraints that return @Just@ something are removed from the transaction.
--
-- The tweak returns a list of all the @Just@ values that were returned by the
-- removed constraints.
removeOutConstraintsTweak :: (OutConstraint -> Maybe a) -> Tweak [a]
removeOutConstraintsTweak removePred = do
  mcs <- viewTweak outConstraintsL
  let (removed, kept) = partitionMaybe removePred mcs
  setTweak outConstraintsL kept
  return removed
