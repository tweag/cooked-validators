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
incompletely. It succeeds whenever the requirements of two validators (which
often are the same) ovelap, but the outputs of the transaction are not
sufficiently unique, so that both validators see the outputs as satisfying
"their" requirement.

The mechanism is explained very well in the following analogy from the Plutus
documentation: "Suppose that two tax auditors from two different departments
come to visit you in turn to see if you’ve paid your taxes. You come up with a
clever scheme to confuse them. Your tax liability to both departments is $10, so
you make a single payment to the tax office’s bank account for $10. When the
auditors arrive, you show them your books, containing the payment to the tax
office. They both leave satisfied."

The double satisfaction attack 'doubleSatAttack' provided by this module works
by adding some extra 'Constraints' that depend on 'SpendsScript' constraints on
the transaction under modification and the current 'MockChainSt'ate. This very
general concept allows the implementation of common double satisfaction patterns
as special cases, which will correspond to smart constructors for the
'DoubleSatParams' type.
-}

-- | Parameters for a double satisfaction attack. You probably don't want to
-- initialise these by hand; better use one of the smart constructors defined
-- below.
data DoubleSatParams = DoubleSatParams
  { -- | For every 'SpendsScriptConstraint' in the transaction under
    -- modification, calculate a list of extra 'Constraints' to add to the
    -- transaction in order to try a double satisfaction attack.
    --
    -- Extra 'Constraints' are added to pre-existing 'Constraints' with the
    -- function 'joinConstraints', which makes sure that no additional
    -- constraint tries to an UTxO that's already being spent and other such
    -- checks. See the documentation comments for that function below.
    --
    -- We add 'OutConstraint's at the end of the list, because some contracts
    -- rely on the ordering of (the first few) transaction outputs. If you want
    -- to try permutations of transaction outputs, look at the function
    -- 'tryOutPermutations', which is an attack-agnostic way to accomplish that.
    dsExtraConstraints :: MockChainSt -> SpendsScriptConstraint -> [Constraints],
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
    --   adding _at least one_ extra 'Constraints' belonging to an original
    --   'SpendsScript' constraints are tried: If there are n 'SpendsScript'
    --   constraints in the original transaction, the modified transactions will
    --   incorporate up to n extra 'Constraints'. For a given 'SpendsScript'
    --   constraint on the original transaction, each of its extra 'Constraints'
    --   is tried together with all options for all other 'SpendsScript'
    --   constraints. This means that all cases that the 'OneChange'-case checks
    --   are also explored here, plus some (potentially very many) more.
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

-- | Parameters for a double satisfaction attack that adds one extra input
-- belonging to a given validator to transactions. Each of the modified
-- transactions will contain exactly one extra 'SpendsScript' constraint.
dsOneExtraInputFrom ::
  ( SpendsConstrs b,
    Pl.FromData (L.DatumType b)
  ) =>
  -- | The validator to take extra inputs from.
  L.TypedValidator b ->
  -- | For all 'SpendsScript' constraints of the original transaction, decide
  -- whether to add an extra UTxO, and if so, which redeemers to try. Each
  -- redeemer is tried on a separate output transaction.
  (SpendsScriptConstraint -> (SpendableOut, L.DatumType b) -> [L.RedeemerType b]) ->
  -- | Wallet of the attacker. Any value contained in the extra UTxO consumed by
  -- the modified transactions is paid to this wallet.
  Wallet ->
  DoubleSatParams
dsOneExtraInputFrom extraInputOwner extraInputRedeemers attacker =
  DoubleSatParams
    { dsExtraConstraints = \mcst ssc ->
        let extraUtxos = scriptUtxosSuchThatMcst mcst extraInputOwner (\_ _ -> True)
         in concatMap
              ( \utxo ->
                  map
                    (\r -> toConstraints $ SpendsScript extraInputOwner r utxo)
                    (extraInputRedeemers ssc utxo)
              )
              extraUtxos,
      dsAttacker = attacker,
      dsSplitStrategy = OneChange
    }

-- | Double satisfaction attack. See the comments for 'DoubleSatParams' and its
-- smart constructors for explanation.
doubleSatAttack ::
  DoubleSatParams ->
  Attack
doubleSatAttack DoubleSatParams {..} =
  mkSplittingAttack
    dsSplitStrategy
    spendsScriptConstraintsT
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
                  over txConstraintsL (extraConstrs `joinConstraints`) skelOld
              | extraConstrs `strictlyExtendsConstraints` (skelOld ^. txConstraintsL)
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
    accConstraints (c : cs) acc = c `joinConstraints` accConstraints cs acc

-- | Extend the constraints in the second argument with the ones from the first
-- argument, but only if there are no conflicts:
--
-- - No 'SpendsScript' or 'SpendsPK' constraints trying to spend the same UTxO
--
-- - No incompatible time ranges
joinConstraints :: Constraints -> Constraints -> Constraints
joinConstraints (extraIs :=>: extraOs) r@(is :=>: os) =
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
  if mc `elem` mcs
    then AlreadyThere
    else
      let spends :: SpendableOut -> MiscConstraint -> Bool
          spends o (SpendsScript _ _ (o', _)) = o == o'
          spends o (SpendsPK o') = o == o'
          spends _ _ = False

          validRange :: [MiscConstraint] -> Pl.POSIXTimeRange
          validRange [] = Pl.always
          validRange (Before t : ms) = Pl.to t `Pl.intersection` validRange ms
          validRange (After t : ms) = Pl.from t `Pl.intersection` validRange ms
          validRange (ValidateIn range : ms) = range `Pl.intersection` validRange ms
          validRange (_ : ms) = validRange ms
       in case mc of
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
            _ -> CompatibleExtension

data DoubleSatLbl = DoubleSatLbl
  deriving (Eq, Show)
