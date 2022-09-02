{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cooked.Attack.DoubleSat where

import Control.Monad
import Cooked.Attack.Common
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.List
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import Optics.Core
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V1.Ledger.Time as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl
import qualified PlutusTx.Prelude as Pl

removeMiscConstraintsAttack :: (MiscConstraint -> Bool) -> Attack [MiscConstraint]
removeMiscConstraintsAttack removePred = do
  mcs <- viewAttack miscConstraintsL
  let (removed, kept) = partition removePred mcs
  setAttack miscConstraintsL kept
  return removed

-- | This attack ensures that a certain 'SpendsScript' constraint is present on
-- a transaction.
--
-- Fails if the UTxO that should be spent is already consumed by the
-- transaction, either using a different redeemer, or assuming a different
-- datum.
--
-- Returns @Just@ a tuple of its input arguments iff the transaction was
-- modified.
addSpendsScriptAttack ::
  forall a.
  SpendsConstrs a =>
  L.TypedValidator a ->
  L.RedeemerType a ->
  (SpendableOut, L.DatumType a) ->
  Attack (Maybe (L.TypedValidator a, L.RedeemerType a, (SpendableOut, L.DatumType a)))
addSpendsScriptAttack v r (o, d) = do
  present <- viewAttack (partsOf $ spendsScriptConstraintsT % spendsScriptConstraintTypeP @a)
  let clashing = filter (\(_, _, (o', _)) -> o == o') present
  case clashing of
    [] -> do
      overAttack miscConstraintsL (SpendsScript v r (o, d) :)
      return $ Just (v, r, (o, d))
    [(v', r', (_, d'))] ->
      if v' == v && r' Pl.== r && d' Pl.== d
        then return Nothing
        else failingAttack
    _ -> return Nothing -- Something's already wrong with the unmodified
    -- transaction, since it spends the same UTxO at least twice. Let's not fail
    -- nonetheless, maybe there's a reason for the madness.

-- | This attack makes sure that a certain 'SpendsPK' constraint is present on a
-- transaction.
--
-- Returns @Just@ the added 'SpendableOut' iff the transaction was changed.
addSpendsPKAttack :: SpendableOut -> Attack (Maybe SpendableOut)
addSpendsPKAttack extraUtxo = do
  consumed <- viewAttack (partsOf $ miscConstraintT % spendsPKConstraintP)
  if extraUtxo `elem` consumed
    then return Nothing
    else do
      overAttack miscConstraintsL (SpendsPK extraUtxo :)
      return $ Just extraUtxo

-- | This attack adds a 'Mints' constraint.
addMintsAttack ::
  MintsConstrs a =>
  Maybe a ->
  [L.MintingPolicy] ->
  L.Value ->
  Attack ()
addMintsAttack r ps x = overAttack miscConstraintsL (Mints r ps x :)

-- | This attack removes all time constraints (i.e. 'Before', 'After', and
-- 'ValidateIn') from a transaction. It returns the validity time range of the
-- unmodified transaction.
removeTimeConstraintsAttack :: Attack Pl.POSIXTimeRange
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
          -- I don't know if 'Before' and 'After' should
          -- include the endpoint or not. I decided against the
          -- inclusion. TODO
          Before b -> Pl.intersection (Pl.to (b - 1))
          After a -> Pl.intersection (Pl.from (a + 1))
          ValidateIn i -> Pl.intersection i
          _ -> id
      )
      Pl.always
      timeConstraints

-- | This attack restricts the validity time range of a transaction to the
-- intersection of the given range and its current validity time range. It also
-- "cleans up" the time constraints (i.e. 'Before', 'After', and 'ValidateIn'),
-- so that the resulting transaction has only one 'ValidateIn' constraint.
--
-- Returns the old validity time range of the transaction.
addValidateInAttack :: Pl.POSIXTimeRange -> Attack Pl.POSIXTimeRange
addValidateInAttack range = do
  oldRange <- removeTimeConstraintsAttack
  overAttack miscConstraintsL (ValidateIn (range `Pl.intersection` oldRange) :)
  return oldRange

-- | This attack removes all 'SignedBy' constraints from a transaction. It
-- returns the list of the signers it removed.
removeSignedByAttack :: Attack [L.PubKeyHash]
removeSignedByAttack = do
  removed <- removeMiscConstraintsAttack (\case SignedBy _ -> True; _ -> False)
  return $ concatMap (\case SignedBy s -> s; _ -> []) removed

-- | This attack adds signers to a transaction. It also combines all 'SignedBy'
-- constraints of the transaction into one.
--
-- Returns a list of all signers that were added (and were not already present
-- on the transaction).
addSignedByAttack :: [L.PubKeyHash] -> Attack [L.PubKeyHash]
addSignedByAttack signers = do
  oldSigners <- removeSignedByAttack
  let newSigners = filter (`notElem` oldSigners) signers
  overAttack miscConstraintsL (SignedBy (newSigners ++ oldSigners) :)
  return newSigners

-- | This attack adds 'MiscConstraint's to a transaction, in a way that avoids
-- conflicts and also potentially simplifies the constraints. In particular,
--
-- - if a 'SpendsPK' constraint is added that consumes an UTxO already consumed
--   by a 'SpendsPK', the attack fails,
--
-- - if a 'SpendsScript' is added that consumes an UTxO already specified to be
--   consumed by the transaction, but with a different redeemer, or assuming a
--   different datum, the attack fails,
--
-- - 'Mints' constraints are just added without any additional checks,
--
-- - if any constraint concerning time (i.e. 'Before', 'After', or 'ValidateIn')
--   is added, the resulting transaction will only have one 'ValidateIn'
--   constraint, specifying as the validity time range the intersection of all
--   the pre-existing time ranges on the transaction and the new time range,
--
-- - if a 'SignedBy' constraint is added, the resulting transaction will
--   similarly have only one 'SignedBy' constraint, which holds all of the
--   signers required by the unmodified transaction, together with the new
--   signers.
--
-- The returned 'MiscConstraint' is the constraint that was added to the
-- transaction.
addMiscConstraintAttack :: MiscConstraint -> Attack (Maybe MiscConstraint)
addMiscConstraintAttack (SpendsScript v r (o, d)) =
  do
    added <- addSpendsScriptAttack v r (o, d)
    case added of
      Nothing -> return Nothing
      Just (v', r', (o', d')) -> return $ Just $ SpendsScript v' r' (o', d')
addMiscConstraintAttack (SpendsPK o) =
  addSpendsPKAttack o
    >>= maybe (return Nothing) (return . Just . SpendsPK)
addMiscConstraintAttack (Mints r ps x) =
  addMintsAttack r ps x
    >> return (Just $ Mints r ps x)
addMiscConstraintAttack (Before b) =
  let leftUnbounded = Pl.to $ b - 1 -- is this correct, i.e. should the end time be excluded?
   in addValidateInAttack leftUnbounded
        >>= return . Just . ValidateIn . Pl.intersection leftUnbounded
addMiscConstraintAttack (After a) =
  let rightUnbounded = Pl.from $ a + 1 -- is this correct, i.e. should the start time be excluded?
   in addValidateInAttack rightUnbounded
        >>= return . Just . ValidateIn . Pl.intersection rightUnbounded
addMiscConstraintAttack (ValidateIn range) =
  addValidateInAttack range
    >>= return . Just . ValidateIn . Pl.intersection range
addMiscConstraintAttack (SignedBy s) = addSignedByAttack s >>= return . Just . SignedBy

addConstraintsAttack :: Constraints -> Attack Constraints
addConstraintsAttack (is :=>: os) = do
  addedMiscConstraints <- mapM addMiscConstraintAttack is
  overAttack outConstraintsL (++ os) -- appended at the end
  return (catMaybes addedMiscConstraints :=>: os)

data SplitMode = Combine | OneOutputPerFocus

doubleSatAttack ::
  Is k A_Traversal =>
  Optic' k is TxSkel a ->
  (MockChainSt -> a -> Constraints) ->
  SplitMode ->
  Attack ()
doubleSatAttack optic extraConstrs mode = do
  foci <- viewAttack (partsOf optic)
  mcst <- mcstAttack
  let cs = map (extraConstrs mcst) foci

  return ()

-- ensureConstraintsAttack :: Constraints -> Attack (Maybe Constraints)
-- ensureConstraintsAttack (is :=>: os) = do

-- {- Note: What is a double satisfaction attack?

-- A double satisfaction attack consists in trying to satisfy the requirements for
-- what conceptually are two transactions in a single transaction, and doing so
-- incompletely. It succeeds whenever the requirements of two validators ovelap,
-- but the required outputs of the transaction are not sufficiently unique, so that
-- both validators see them as satisfying "their" requirement.

-- The mechanism is explained very well in the following analogy from the Plutus
-- documentation: "Suppose that two tax auditors from two different departments
-- come to visit you in turn to see if you’ve paid your taxes. You come up with a
-- clever scheme to confuse them. Your tax liability to both departments is $10, so
-- you make a single payment to the tax office’s bank account for $10. When the
-- auditors arrive, you show them your books, containing the payment to the tax
-- office. They both leave satisfied."

-- The double satisfaction attack 'doubleSatAttack' provided by this module works
-- by adding some extra 'Constraints' depending on some 'a's on the transaction
-- under modification, and on the current 'MockChainSt'ate. This very general
-- concept allows the implementation of common double satisfaction patterns as
-- special cases, which will correspond to smart constructors for the
-- 'DoubleSatParams' type. In particular, we can add

-- - a 'SpendsScript', depending on a 'SpendsScript' on the original transaction,
-- - one or more 'Mints', depending on a 'SpendsScript' on the original transaction,
-- - a 'SpendsScript', depending on a 'PaysScript' on the original transaction,
-- - ...

-- A few scenarios like these are implemented below as smart constructors for
-- 'DoubleSatParams'.

-- -}

-- -- | Parameters for a double satisfaction attack. You probably don't want to
-- -- initialise these by hand; better use one of the smart constructors defined
-- -- below.
-- data DoubleSatParams a = DoubleSatParams
--   { -- | Each focus of this traversal is one potential reason to add extra
--     -- 'Constraint's.
--     dsOptic :: Traversal' TxSkel a,
--     -- | For every 'a' in the transaction under modification, calculate a list
--     -- of extra 'Constraints' to add to the transaction in order to try a double
--     -- satisfaction attack.
--     --
--     -- Extra 'Constraints' are added to pre-existing 'Constraints' with the
--     -- function 'addConstraints', which makes sure that no additional constraint
--     -- tries spending an UTxO that's already being spent and other such
--     -- checks. See the documentation comments for that function below.
--     --
--     -- We add 'OutConstraint's at the end of the list of transaction outputs,
--     -- because some contracts rely on the ordering of (the first few)
--     -- outputs. If you want to try permutations of transaction outputs, look at
--     -- the function 'tryOutPermutations', which is an attack-agnostic way to
--     -- accomplish that.
--     dsExtraConstraints :: MockChainSt -> a -> [Constraints],
--     -- | The wallet to which any surplus will be paid.
--     dsAttacker :: Wallet,
--     -- | How to combine extra 'Constraint's:
--     --
--     -- There are at the moment the following two strategies for how to combine
--     -- additional 'Constraints' belonging to _different_ 'SpendsScript'
--     -- constraints of the original transaction.
--     --
--     -- - With @dsSplitStrategy = OneChange@, each extra 'Constraints' is added
--     --   onto a separate modified 'TxSkel': Each of the output modified
--     --   'TxSkel's will receive exactly one of the extra 'Constraints'.
--     --
--     -- - With @dsSplitStrategy = AllCombinations@, al 'TxSkels' obtained by
--     --   adding _at least one_ extra 'Constraints' belonging to an original 'a'
--     --   are tried: If there are n 'a's in the original transaction, the
--     --   modified transactions will incorporate up to n extra 'Constraints'. For
--     --   a given 'a' on the original transaction, each of its extra
--     --   'Constraints' is tried together with all options for all other
--     --   'a's. This means that all cases that the 'OneChange'-case checks are
--     --   also explored here, plus some (potentially very many) more.
--     --
--     -- If there is a conflict between some of the 'Constraints' that are to be
--     -- added to an output transaction (for example, two 'Constraints' might
--     -- require the same UTxO to be spent with different redeemers), that
--     -- combination is not tried.
--     --
--     -- See the comments for 'mkSplittingAttack' for more explanation on
--     -- 'SplitStrategy'.
--     dsSplitStrategy :: SplitStrategy
--   }

-- -- * Smart constructors for 'DoubleSatParams'

-- -- | Parameters for a double satisfaction attack that adds one
-- -- 'SpendsScriptConstraint' for a UTxO belonging to a given validator. Each of
-- -- the modified transactions will contain exactly one extra 'SpendsScript'
-- -- constraint.
-- dsAddOneSscFromOwner ::
--   ( SpendsConstrs b,
--     Pl.FromData (L.DatumType b)
--   ) =>
--   Traversal' TxSkel a ->
--   -- | The validator to take extra inputs from.
--   L.TypedValidator b ->
--   -- | For all 'a's of the original transaction, decide whether to add an extra
--   -- UTxO currently belonging to the @extraInputOwner@, and if so, which
--   -- redeemers to try. Each redeemer is tried on a separate output transaction.
--   (a -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
--   -- Wallet of the attacker. Any value contained in the extra UTxO consumed by
--   -- the modified transaction is psid to this wallet.
--   Wallet ->
--   DoubleSatParams a
-- dsAddOneSscFromOwner optic extraInputOwner extraInputRedeemers attacker =
--   dsAddSsc
--     optic
--     ( \mcst a ->
--         let extraUtxos = scriptUtxosSuchThatMcst mcst extraInputOwner (\_ _ -> True)
--          in concatMap
--               ( \utxo ->
--                   map
--                     (\r -> SpendsScriptConstraint extraInputOwner r utxo)
--                     (extraInputRedeemers a utxo)
--               )
--               extraUtxos
--     )
--     attacker
--     OneChange

-- -- | Parameters for a double satisfaction attack that adds one extra
-- -- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- -- transactions, depending on 'PaysScript' constraints already present on the
-- -- transaction.  Each of the modified transactions will contain exactly one
-- -- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- -- for explanation of the arguments.
-- dsAddOneSscToPsc ::
--   ( SpendsConstrs b,
--     Pl.FromData (L.DatumType b)
--   ) =>
--   L.TypedValidator b ->
--   (PaysScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
--   Wallet ->
--   DoubleSatParams PaysScriptConstraint
-- dsAddOneSscToPsc = dsAddOneSscFromOwner paysScriptConstraintsT

-- -- | Parameters for a double satisfaction attack that adds one extra
-- -- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- -- transactions, depending on 'SpendsScript' constraints already present on the
-- -- transaction.  Each of the modified transactions will contain exactly one
-- -- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- -- for explanation of the arguments.
-- dsAddOneSscToSsc ::
--   ( SpendsConstrs b,
--     Pl.FromData (L.DatumType b)
--   ) =>
--   L.TypedValidator b ->
--   (SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
--   Wallet ->
--   DoubleSatParams SpendsScriptConstraint
-- dsAddOneSscToSsc = dsAddOneSscFromOwner spendsScriptConstraintsT

-- -- | Parameters for a double satisfaction attack that adds one extra
-- -- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- -- transactions, depending on 'Mints' constraints already present on the
-- -- transaction.  Each of the modified transactions will contain exactly one
-- -- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- -- for explanation of the arguments.
-- dsAddOneSscToMc ::
--   ( SpendsConstrs b,
--     Pl.FromData (L.DatumType b)
--   ) =>
--   L.TypedValidator b ->
--   (MintsConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
--   Wallet ->
--   DoubleSatParams MintsConstraint
-- dsAddOneSscToMc = dsAddOneSscFromOwner mintsConstraintsT

-- -- | Parameters for a double satisfaction attack that adds one or more extra
-- -- 'Mints' constraints to transactions, depending on 'SpendsScript' constraints
-- -- already present on the transaction.  Each of the modified transactions will
-- -- contain at least one extra 'Mints' constraint.
-- dsAddMcToSsc ::
--   MintsConstrs a =>
--   -- | For all 'SpendsScript' constraints of the original transaction, decide
--   -- whether to mint some extra value, and which redeemer and minting policies
--   -- to use to do so.
--   (SpendsScriptConstraint -> [(Maybe a, [L.MintingPolicy], L.Value)]) ->
--   -- | Wallet of the attacker. Any extra minted values are paid to this wallet.
--   Wallet ->
--   DoubleSatParams SpendsScriptConstraint
-- dsAddMcToSsc extraMints attacker =
--   dsAddMc
--     spendsScriptConstraintsT
--     (\_ ssc -> map (\(r, ps, x) -> MintsConstraint r ps x) $ extraMints ssc)
--     attacker
--     AllCombinations

-- -- | Parameters for a double satisfaction attack that adds one or more extra
-- -- 'Mints' constraints to transactions, depending on 'Mints' constraints
-- -- already present on the transaction.  Each of the modified transactions will
-- -- contain at least one extra 'Mints' constraint.
-- --
-- -- Note that this is more general than just a token duplication attack as
-- -- implemented by 'dupTokenAttack': That attack is used to make _one_ minting
-- -- policy mint more tokens, with this attack, more than one minting policy may
-- -- be involved.
-- dsAddMcToMc ::
--   MintsConstrs a =>
--   -- | For all 'Mints' constraints of the original transaction, decide
--   -- whether to mint some extra value, and which redeemer and minting policies
--   -- to use to do so.
--   (MintsConstraint -> [(Maybe a, [L.MintingPolicy], L.Value)]) ->
--   -- | Wallet of the attacker. Any extra minted values are paid to this wallet.
--   Wallet ->
--   DoubleSatParams MintsConstraint
-- dsAddMcToMc extraMints attacker =
--   dsAddMc
--     mintsConstraintsT
--     (\_ mc -> map (\(r, ps, x) -> MintsConstraint r ps x) $ extraMints mc)
--     attacker
--     AllCombinations

-- -- | Parameters for a double satisfaction attack that adds 'SpendsScript'
-- -- constraints.
-- dsAddSsc ::
--   Traversal' TxSkel a ->
--   (MockChainSt -> a -> [SpendsScriptConstraint]) ->
--   Wallet ->
--   SplitStrategy ->
--   DoubleSatParams a
-- dsAddSsc optic extraSsc attacker splitStrategy =
--   DoubleSatParams
--     { dsOptic = optic,
--       dsExtraConstraints = \mcst a ->
--         map
--           ( toConstraints
--               . review spendsScriptConstraintP
--           )
--           $ extraSsc mcst a,
--       dsAttacker = attacker,
--       dsSplitStrategy = splitStrategy
--     }

-- -- | Parameters for a doubleSatisfactionAttack that adds 'Mints' constraints.
-- dsAddMc ::
--   Traversal' TxSkel a ->
--   (MockChainSt -> a -> [MintsConstraint]) ->
--   Wallet ->
--   SplitStrategy ->
--   DoubleSatParams a
-- dsAddMc optic extraMc attacker splitStrategy =
--   DoubleSatParams
--     { dsOptic = optic,
--       dsExtraConstraints = \mcst a ->
--         map
--           ( toConstraints
--               . review mintsConstraintP
--           )
--           $ extraMc mcst a,
--       dsAttacker = attacker,
--       dsSplitStrategy = splitStrategy
--     }

-- -- * The double satisfaction attack

-- -- | Double satisfaction attack. See the comments for 'DoubleSatParams' and its
-- -- smart constructors for explanation.
-- doubleSatAttack ::
--   DoubleSatParams a ->
--   Attack
-- doubleSatAttack DoubleSatParams {..} =
--   mkSplittingAttack
--     dsSplitStrategy
--     dsOptic
--     ( \mcst ssc ->
--         map
--           (ssc,)
--           (dsExtraConstraints mcst ssc)
--     )
--     ( \extraConstrList skelOld ->
--         let extraConstrs = accConstraints extraConstrList ([] :=>: [])
--             extraInValue = foldOf (constraintPairI % _1 % traversed % valueAT) extraConstrs
--             extraOutValue = foldOf (constraintPairI % _2 % traversed % valueL) extraConstrs
--             surplus = extraInValue <> Pl.negate extraOutValue
--          in [ addLabel DoubleSatLbl $
--                 payTo dsAttacker surplus $
--                   over txConstraintsL (extraConstrs `addConstraints`) skelOld
--               | extraConstrs `strictlyExtendsConstraints` txConstraints skelOld
--             ]
--     )
--   where
--     payTo :: Wallet -> L.Value -> TxSkel -> TxSkel
--     payTo w v = over outConstraintsL (++ [paysPK (walletPKHash w) v])

--     -- Accumulate a list of 'Constraints' into a single 'Constraints', in such a
--     -- way that extra 'MiscConstraint's are added in the front (the order should
--     -- not matter), and extra 'OutConstraint's are added in the back (here, the
--     -- order might matter).
--     --
--     -- Note: This combines the list of extra constraints into a
--     accConstraints :: [Constraints] -> Constraints -> Constraints
--     accConstraints [] acc = acc
--     accConstraints (c : cs) acc = c `addConstraints` accConstraints cs acc

-- data DoubleSatLbl = DoubleSatLbl
--   deriving (Eq, Show)
