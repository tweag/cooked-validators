{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cooked.Attack.DoubleSat where

import Cooked.Attack.Common
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import Optics.Core
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V1.Ledger.Time as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

{- Note: What is a double satisfaction attack?

A double satisfaction attack consists in trying to satisfy the requirements for
what conceptually are two transactions in a single transaction, and doing so
incompletely. It succeeds whenever the requirements of two validators ovelap,
but the required outputs of the transaction are not sufficiently unique, so that
both validators see them as satisfying "their" requirement.

The mechanism is explained very well in the following analogy from the Plutus
documentation: "Suppose that two tax auditors from two different departments
come to visit you in turn to see if you’ve paid your taxes. You come up with a
clever scheme to confuse them. Your tax liability to both departments is $10, so
you make a single payment to the tax office’s bank account for $10. When the
auditors arrive, you show them your books, containing the payment to the tax
office. They both leave satisfied."

The double satisfaction attack 'doubleSatAttack' provided by this module works
by adding some extra 'Constraints' depending on some 'a's on the transaction
under modification, and on the current 'MockChainSt'ate. This very general
concept allows the implementation of common double satisfaction patterns as
special cases, which will correspond to smart constructors for the
'DoubleSatParams' type. In particular, we can add

- a 'SpendsScript', depending on a 'SpendsScript' on the original transaction,
- one or more 'Mints', depending on a 'SpendsScript' on the original transaction,
- a 'SpendsScript', depending on a 'PaysScript' on the original transaction,
- ...

A few scenarios like these are implemented below as smart constructors for
'DoubleSatParams'.

-}

-- | Parameters for a double satisfaction attack. You probably don't want to
-- initialise these by hand; better use one of the smart constructors defined
-- below.
data DoubleSatParams a = DoubleSatParams
  { -- | Each focus of this traversal is one potential reason to add extra
    -- 'Constraint's.
    dsOptic :: Traversal' TxSkel a,
    -- | For every 'a' in the transaction under modification, calculate a list
    -- of extra 'Constraints' to add to the transaction in order to try a double
    -- satisfaction attack.
    --
    -- Extra 'Constraints' are added to pre-existing 'Constraints' with the
    -- function 'addConstraints', which makes sure that no additional constraint
    -- tries spending an UTxO that's already being spent and other such
    -- checks. See the documentation comments for that function below.
    --
    -- We add 'OutConstraint's at the end of the list of transaction outputs,
    -- because some contracts rely on the ordering of (the first few)
    -- outputs. If you want to try permutations of transaction outputs, look at
    -- the function 'tryOutPermutations', which is an attack-agnostic way to
    -- accomplish that.
    dsExtraConstraints :: MockChainSt -> a -> [Constraints],
    -- | The wallet to which any surplus will be paid.
    dsAttacker :: Wallet,
    -- | How to combine extra 'Constraint's:
    --
    -- There are at the moment the following two strategies for how to combine
    -- additional 'Constraints' belonging to _different_ 'SpendsScript'
    -- constraints of the original transaction.
    --
    -- - With @dsSplitStrategy = OneChange@, each extra 'Constraints' is added
    --   onto a separate modified 'TxSkel': Each of the output modified
    --   'TxSkel's will receive exactly one of the extra 'Constraints'.
    --
    -- - With @dsSplitStrategy = AllCombinations@, al 'TxSkels' obtained by
    --   adding _at least one_ extra 'Constraints' belonging to an original 'a'
    --   are tried: If there are n 'a's in the original transaction, the
    --   modified transactions will incorporate up to n extra 'Constraints'. For
    --   a given 'a' on the original transaction, each of its extra
    --   'Constraints' is tried together with all options for all other
    --   'a's. This means that all cases that the 'OneChange'-case checks are
    --   also explored here, plus some (potentially very many) more.
    --
    -- If there is a conflict between some of the 'Constraints' that are to be
    -- added to an output transaction (for example, two 'Constraints' might
    -- require the same UTxO to be spent with different redeemers), that
    -- combination is not tried.
    --
    -- See the comments for 'mkSplittingAttack' for more explanation on
    -- 'SplitStrategy'.
    dsSplitStrategy :: SplitStrategy
  }

-- * Smart constructors for 'DoubleSatParams'

-- | Parameters for a double satisfaction attack that adds one
-- 'SpendsScriptConstraint' for a UTxO belonging to a given validator. Each of
-- the modified transactions will contain exactly one extra 'SpendsScript'
-- constraint.
dsAddOneSscFromOwner ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  Traversal' TxSkel a ->
  -- | The validator to take extra inputs from.
  L.TypedValidator b ->
  -- | For all 'a's of the original transaction, decide whether to add an extra
  -- UTxO currently belonging to the @extraInputOwner@, and if so, which
  -- redeemers to try. Each redeemer is tried on a separate output transaction.
  (a -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  -- Wallet of the attacker. Any value contained in the extra UTxO consumed by
  -- the modified transaction is psid to this wallet.
  Wallet ->
  DoubleSatParams a
dsAddOneSscFromOwner optic extraInputOwner extraInputRedeemers attacker =
  dsAddSsc
    optic
    ( \mcst a ->
        let extraUtxos = scriptUtxosSuchThatMcst mcst extraInputOwner (\_ _ -> True)
         in concatMap
              ( \utxo ->
                  map
                    (\r -> SpendsScriptConstraint extraInputOwner r utxo)
                    (extraInputRedeemers a utxo)
              )
              extraUtxos
    )
    attacker
    OneChange

-- | Parameters for a double satisfaction attack that adds one extra
-- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- transactions, depending on 'PaysScript' constraints already present on the
-- transaction.  Each of the modified transactions will contain exactly one
-- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- for explanation of the arguments.
dsAddOneSscToPsc ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  L.TypedValidator b ->
  (PaysScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  Wallet ->
  DoubleSatParams PaysScriptConstraint
dsAddOneSscToPsc = dsAddOneSscFromOwner paysScriptConstraintsT

-- | Parameters for a double satisfaction attack that adds one extra
-- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- transactions, depending on 'SpendsScript' constraints already present on the
-- transaction.  Each of the modified transactions will contain exactly one
-- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- for explanation of the arguments.
dsAddOneSscToSsc ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  L.TypedValidator b ->
  (SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  Wallet ->
  DoubleSatParams SpendsScriptConstraint
dsAddOneSscToSsc = dsAddOneSscFromOwner spendsScriptConstraintsT

-- | Parameters for a double satisfaction attack that adds one extra
-- 'SpendsScript' constraint for a Utxo belonging to a given validator to
-- transactions, depending on 'Mints' constraints already present on the
-- transaction.  Each of the modified transactions will contain exactly one
-- extra 'SpendsScript' constraint. See the comments at 'dsAddOneSscFromOwner'
-- for explanation of the arguments.
dsAddOneSscToMc ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  L.TypedValidator b ->
  (MintsConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  Wallet ->
  DoubleSatParams MintsConstraint
dsAddOneSscToMc = dsAddOneSscFromOwner mintsConstraintsT

-- | Parameters for a double satisfaction attack that adds one or more extra
-- 'Mints' constraints to transactions, depending on 'SpendsScript' constraints
-- already present on the transaction.  Each of the modified transactions will
-- contain at least one extra 'Mints' constraint.
dsAddMcToSsc ::
  MintsConstrs a =>
  -- | For all 'SpendsScript' constraints of the original transaction, decide
  -- whether to mint some extra value, and which redeemer and minting policies
  -- to use to do so.
  (SpendsScriptConstraint -> [(Maybe a, [L.MintingPolicy], L.Value)]) ->
  -- | Wallet of the attacker. Any extra minted values are paid to this wallet.
  Wallet ->
  DoubleSatParams SpendsScriptConstraint
dsAddMcToSsc extraMints attacker =
  dsAddMc
    spendsScriptConstraintsT
    (\_ ssc -> map (\(r, ps, x) -> MintsConstraint r ps x) $ extraMints ssc)
    attacker
    AllCombinations

-- | Parameters for a double satisfaction attack that adds one or more extra
-- 'Mints' constraints to transactions, depending on 'Mints' constraints
-- already present on the transaction.  Each of the modified transactions will
-- contain at least one extra 'Mints' constraint.
--
-- Note that this is more general than just a token duplication attack as
-- implemented by 'dupTokenAttack': That attack is used to make _one_ minting
-- policy mint more tokens, with this attack, more than one minting policy may
-- be involved.
dsAddMcToMc ::
  MintsConstrs a =>
  -- | For all 'Mints' constraints of the original transaction, decide
  -- whether to mint some extra value, and which redeemer and minting policies
  -- to use to do so.
  (MintsConstraint -> [(Maybe a, [L.MintingPolicy], L.Value)]) ->
  -- | Wallet of the attacker. Any extra minted values are paid to this wallet.
  Wallet ->
  DoubleSatParams MintsConstraint
dsAddMcToMc extraMints attacker =
  dsAddMc
    mintsConstraintsT
    (\_ mc -> map (\(r, ps, x) -> MintsConstraint r ps x) $ extraMints mc)
    attacker
    AllCombinations

-- | Parameters for a double satisfaction attack that adds 'SpendsScript'
-- constraints.
dsAddSsc ::
  Traversal' TxSkel a ->
  (MockChainSt -> a -> [SpendsScriptConstraint]) ->
  Wallet ->
  SplitStrategy ->
  DoubleSatParams a
dsAddSsc optic extraSsc attacker splitStrategy =
  DoubleSatParams
    { dsOptic = optic,
      dsExtraConstraints = \mcst a ->
        map
          ( toConstraints
              . review spendsScriptConstraintP
          )
          $ extraSsc mcst a,
      dsAttacker = attacker,
      dsSplitStrategy = splitStrategy
    }

-- | Parameters for a doubleSatisfactionAttack that adds 'Mints' constraints.
dsAddMc ::
  Traversal' TxSkel a ->
  (MockChainSt -> a -> [MintsConstraint]) ->
  Wallet ->
  SplitStrategy ->
  DoubleSatParams a
dsAddMc optic extraMc attacker splitStrategy =
  DoubleSatParams
    { dsOptic = optic,
      dsExtraConstraints = \mcst a ->
        map
          ( toConstraints
              . review mintsConstraintP
          )
          $ extraMc mcst a,
      dsAttacker = attacker,
      dsSplitStrategy = splitStrategy
    }

-- * The double satisfaction attack

-- | Double satisfaction attack. See the comments for 'DoubleSatParams' and its
-- smart constructors for explanation.
doubleSatAttack ::
  DoubleSatParams a ->
  Attack
doubleSatAttack DoubleSatParams {..} =
  mkSplittingAttack
    dsSplitStrategy
    dsOptic
    ( \mcst ssc ->
        map
          (ssc,)
          (dsExtraConstraints mcst ssc)
    )
    ( \extraConstrList skelOld ->
        let extraConstrs = accConstraints extraConstrList ([] :=>: [])
            extraInValue = foldOf (constraintPairI % _1 % traversed % valueAT) extraConstrs
            extraOutValue = foldOf (constraintPairI % _2 % traversed % valueL) extraConstrs
            surplus = extraInValue <> Pl.negate extraOutValue
         in [ addLabel DoubleSatLbl $
                payTo dsAttacker surplus $
                  over txConstraintsL (extraConstrs `addConstraints`) skelOld
              | extraConstrs `strictlyExtendsConstraints` txConstraints skelOld
            ]
    )
  where
    payTo :: Wallet -> L.Value -> TxSkel -> TxSkel
    payTo w v = over outConstraintsL (++ [paysPK (walletPKHash w) v])

    -- Accumulate a list of 'Constraints' into a single 'Constraints', in such a
    -- way that extra 'MiscConstraint's are added in the front (the order should
    -- not matter), and extra 'OutConstraint's are added in the back (here, the
    -- order might matter).
    --
    -- Note: This combines the list of extra constraints into a
    accConstraints :: [Constraints] -> Constraints -> Constraints
    accConstraints [] acc = acc
    accConstraints (c : cs) acc = c `addConstraints` accConstraints cs acc

-- * Extending 'Constraints'

-- | Extend the constraints in the second argument with the ones from the first
-- argument, but only if there are no conflicts:
--
-- - No 'SpendsScript' or 'SpendsPK' constraints trying to spend the same UTxO
--
-- - No incompatible time ranges
addConstraints :: Constraints -> Constraints -> Constraints
addConstraints (extraIs :=>: extraOs) r@(is :=>: os) =
  case miscListExtend extraIs is of
    Nothing -> r
    Just is' -> is' :=>: (os ++ extraOs)
  where
    miscListExtend :: [MiscConstraint] -> [MiscConstraint] -> Maybe [MiscConstraint]
    miscListExtend [] rs = Just rs
    miscListExtend (m : ls) rs = case miscListExtend ls rs of
      Nothing -> Nothing
      Just ms -> case checkMiscExtend m ms of
        Incompatible -> Nothing
        AlreadyThere -> Just ms
        CompatibleExtension -> Just $ m : ms

-- | Return @True@ iff the constraints in the first argument can be added
-- without conflict to the ones in the second argument, and they are not alreay
-- covered by the constraints in the second argument.
strictlyExtendsConstraints :: Constraints -> Constraints -> Bool
strictlyExtendsConstraints (is :=>: os) (is' :=>: os') =
  case checkMiscListExtend is is' of
    Incompatible -> False
    AlreadyThere -> not (all (`elem` os') os)
    CompatibleExtension -> True
  where
    -- check that all constraints from the first list can be added one after
    -- the other to the second list.
    checkMiscListExtend :: [MiscConstraint] -> [MiscConstraint] -> CheckMiscExtend
    checkMiscListExtend [] _ = AlreadyThere
    checkMiscListExtend (l : ls) rs = case checkMiscExtend l rs of
      Incompatible -> Incompatible
      CompatibleExtension -> case checkMiscListExtend ls (l : rs) of
        Incompatible -> Incompatible
        _ -> CompatibleExtension
      AlreadyThere -> checkMiscListExtend ls (l : rs)

data CheckMiscExtend = Incompatible | AlreadyThere | CompatibleExtension

-- | Check how a given 'MiscConstraint' relates to a set of other
-- 'MiscConstraint's:
--
-- - If the set of constraints already includes the given constraint (either
--   directly or as a consequence), return 'AlreadyThere'
--
-- - If the given constraint can be added to the set without conflict, return 'CompatibleExtension'
--
-- - Otherwise, return 'Incompatible'
checkMiscExtend :: MiscConstraint -> [MiscConstraint] -> CheckMiscExtend
checkMiscExtend _ [] = CompatibleExtension
checkMiscExtend mc mcs =
  case mc of
    -- 'Mints' and 'SignedBy' Constraints can always be added
    Mints {} -> CompatibleExtension
    SignedBy _ -> CompatibleExtension
    _ ->
      if mc `elem` mcs
        then AlreadyThere
        else case mc of
          SpendsScript _ _ (o, _) -> if any (spends o) mcs then Incompatible else CompatibleExtension
          SpendsPK o -> if any (spends o) mcs then Incompatible else CompatibleExtension
          Before t ->
            let oldRange = validRange mcs
                newRange = Pl.to t
             in if
                    | newRange `Pl.contains` oldRange -> AlreadyThere
                    | newRange `Pl.overlaps` oldRange -> CompatibleExtension
                    | otherwise -> Incompatible
          After t ->
            let oldRange = validRange mcs
                newRange = Pl.from t
             in if
                    | newRange `Pl.contains` oldRange -> AlreadyThere
                    | newRange `Pl.overlaps` oldRange -> CompatibleExtension
                    | otherwise -> Incompatible
          ValidateIn newRange ->
            let oldRange = validRange mcs
             in if
                    | newRange `Pl.contains` oldRange -> AlreadyThere
                    | newRange `Pl.overlaps` oldRange -> CompatibleExtension
                    | otherwise -> Incompatible
          _ -> CompatibleExtension -- This case will never be reached
  where
    spends :: SpendableOut -> MiscConstraint -> Bool
    spends o (SpendsScript _ _ (o', _)) = o == o'
    spends o (SpendsPK o') = o == o'
    spends _ _ = False

    validRange :: [MiscConstraint] -> Pl.POSIXTimeRange
    validRange [] = Pl.always
    validRange (Before t : ms) = Pl.to t `Pl.intersection` validRange ms
    validRange (After t : ms) = Pl.from t `Pl.intersection` validRange ms
    validRange (ValidateIn range : ms) = range `Pl.intersection` validRange ms
    validRange (_ : ms) = validRange ms

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
