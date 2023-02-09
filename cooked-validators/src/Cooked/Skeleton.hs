{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Skeleton where

import qualified Cardano.Api as C
import Control.Monad
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Wallet
import Data.Default
import Data.Either.Combinators
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEList
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger.Scripts (validatorHash)
import qualified Ledger.Scripts as Pl hiding (validatorHash)
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core
import Optics.TH
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Value as Pl hiding (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V2.Ledger.Api as Pl hiding (TxOut, adaSymbol, adaToken)
import qualified Plutus.V2.Ledger.Tx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.QuickCheck (NonZero (..))
import Type.Reflection

-- For Template Haskell reasons, the most intersting thing in this module (the
-- definition of 'TxSkel') is at the very bottom of this file
--
-- We'll use TH to generate optics for the types in this file, and the naming
-- convention will be that the optic's names will be the corresponding field's
-- names, followed by
--
-- - an 'L' for lenses
--
-- - an 'AT' for affine traversals

-- * Transaction labels

type LabelConstrs x = (Show x, Typeable x, Eq x, Ord x)

data TxLabel where
  TxLabel :: LabelConstrs x => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

instance Show TxLabel where
  show (TxLabel x) = show x

{- note: How to write 'Ord' instances for types with existential type variables in
 their constructors?

The idea of the 'Ord' instances for 'TxLabel', 'MintsConstraint', and
'TxSkelIn' is illustrated by the instance for 'TxLabel' below: Sort by the
type representation of the existential type variable first, and then by the
concrete value within each of the possible instances of the existential.

This means:

- If the type(representation) of a is strictly smaller than the type of b, then
  a<b.

- If the types of a and b are the same, compare a and b normally.

- If the type of a is strictly greater than the type of b, then a>b.

-}

instance Ord TxLabel where
  compare (TxLabel a) (TxLabel x) =
    case compare (SomeTypeRep (typeOf a)) (SomeTypeRep (typeOf x)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf x of
        Just HRefl -> compare a x
        -- This can never happen, since 'eqTypeRep' is implemented in terms of
        -- '==' on the type representation:
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"

-- * Transaction options

-- | Whether to adjust existing public key outputs during transaction
-- balancing. TODO: Why do we need these two options? Are they just historical
-- baggage?
data BalanceOutputPolicy
  = -- | Try to adjust an existing public key output with the change. If no
    --   suitable output can be found, create a new change output.
    AdjustExistingOutput
  | -- | Do not change the existing outputs, always create a new change
    --   output.
    DontAdjustExistingOutput
  deriving (Eq, Ord, Show)

instance Default BalanceOutputPolicy where
  def = AdjustExistingOutput

-- | Which wallet to use to provide outputs for balancing and collaterals.
-- Either the first signer or an explicit wallet. In the second case, this
-- wallet must be a signer of the transaction.
data BalancingWallet
  = BalanceWithFirstSigner
  | BalanceWith Wallet
  deriving (Eq, Ord, Show)

instance Default BalancingWallet where
  def = BalanceWithFirstSigner

-- | Wraps a function that will be applied to a transaction right before
-- submitting it.
newtype RawModTx
  = -- | Apply modification on transaction after balancing, fee calculation, and
    -- final signing areperformed
    RawModTxAfterBalancing (C.Tx C.BabbageEra -> C.Tx C.BabbageEra)

instance Eq RawModTx where
  _ == _ = False

instance Show RawModTx where
  show (RawModTxAfterBalancing _) = "RawModTxAfterBalancing"

-- | Applies a list of modifications right before the transaction is
-- submitted. The leftmost function in the argument list is applied first.
applyRawModOnBalancedTx :: [RawModTx] -> C.Tx C.BabbageEra -> C.Tx C.BabbageEra
applyRawModOnBalancedTx [] = id
applyRawModOnBalancedTx (RawModTxAfterBalancing f : fs) = applyRawModOnBalancedTx fs . f

-- | Set of options to modify the behavior of generating and validating some transaction. Some of these
-- options only have an effect when running in the 'Plutus.Contract.Contract', some only have an effect when
-- running in 'MockChainT'. If nothing is explicitely stated, the option has an effect independently of the
-- running context.
--
-- TODO Refactor field names to avoid clashes on common terms such as "collateral" or "balance"
data TxOpts = TxOpts
  { -- | Performs an adjustment to unbalanced transactions, making sure every
    -- UTxO that is produced has the necessary minimum amount of Ada.
    --
    -- By default, this is set to @False@, given this is the default behavior in Plutus:
    -- https://github.com/input-output-hk/plutus-apps/issues/143#issuecomment-1013012744
    txOptEnsureMinAda :: Bool,
    -- | When submitting a transaction for real (i.e., running in the 'Plutus.Contract.Contract' monad),
    --  it is common to call 'Plutus.Contract.Request.awaitTxConfirmed' after 'Plutus.Contract.Request.submitTxConstraints'.
    --  If you /do NOT/ wish to do so, please set this to @False@.
    --
    --  /This has NO effect when running outside of 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    txOptAwaitTxConfirmed :: Bool,
    -- | Whether to increase the slot counter automatically on this submission.
    -- This is useful for modelling transactions that could be submitted in parallel in reality, so there
    -- should be no explicit ordering of what comes first. One good example is in the Crowdfunding use case contract.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    txOptAutoSlotIncrease :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted ('txOptEnsureMinAda) and balanced. This is prefixed
    -- with /unsafe/ to draw attention to the fact that modifying a transaction
    -- at that stage might make it invalid. Still, this offers a hook for being
    -- able to alter a transaction in unforeseen ways. It is mostly used to test
    -- contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with @unsafeModTx =
    -- [RawModTxAfterBalancing Debug.Trace.traceShowId]@.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.  By
    -- default, this is set to the empty list.
    --
    -- The leftmost function in the list is applied first.
    txOptUnsafeModTx :: [RawModTx],
    -- | Whether to balance the transaction or not. Balancing
    --  ensures that @input + mint = output + fees + burns@, if you decide to
    --  set @balance = false@ you will have trouble satisfying that equation by
    --  hand because @fees@ are variable. You will likely see a
    --  @ValueNotPreserved@ error and should adjust the fees accordingly.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @True@.
    txOptBalance :: Bool,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @AdjustExistingOutput@.
    txOptBalanceOutputPolicy :: BalanceOutputPolicy,
    -- | Which wallet to use to provide outputs for balancing and collaterals.
    -- Either the first signer by default, or an explicit wallet. In the second
    -- case, this wallet must be a signer of the transaction. This option WILL
    -- NOT ensure that it is added in case it is not already present in the
    -- list of signers.
    txOptBalanceWallet :: BalancingWallet
  }
  deriving (Eq, Show)

instance Default TxOpts where
  def =
    TxOpts
      { txOptEnsureMinAda = False,
        txOptAwaitTxConfirmed = True,
        txOptAutoSlotIncrease = True,
        txOptUnsafeModTx = [],
        txOptBalance = True,
        txOptBalanceOutputPolicy = def,
        txOptBalanceWallet = def
      }

-- * Description of the Minting

type MintsConstrs redeemer =
  ( Pl.ToData redeemer,
    Show redeemer,
    PrettyCooked redeemer,
    Typeable redeemer
  )

data MintsRedeemer where
  NoMintsRedeemer :: MintsRedeemer
  SomeMintsRedeemer :: MintsConstrs redeemer => redeemer -> MintsRedeemer

instance Show MintsRedeemer where
  show NoMintsRedeemer = "NoMintsRedeemer"
  show (SomeMintsRedeemer x) = "(SomeMintsRedeemer " ++ show x ++ ")"

instance Eq MintsRedeemer where
  a == b = compare a b == EQ

instance Ord MintsRedeemer where
  compare NoMintsRedeemer NoMintsRedeemer = EQ
  -- The next two clauses are ugly, but necessary, since minting with no
  -- redeemer is represented as minting with the unit redeemer on-chain.
  compare NoMintsRedeemer (SomeMintsRedeemer a) =
    if Pl.unitRedeemer == Pl.Redeemer (Pl.toBuiltinData a)
      then EQ
      else LT
  compare (SomeMintsRedeemer a) NoMintsRedeemer =
    if Pl.unitRedeemer == Pl.Redeemer (Pl.toBuiltinData a)
      then EQ
      else GT
  compare (SomeMintsRedeemer a) (SomeMintsRedeemer b) =
    case compare (SomeTypeRep $ typeOf a) (SomeTypeRep $ typeOf b) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf b of
        Just HRefl -> compare (Pl.toData a) (Pl.toData b)
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"

-- | A description of what a transaction mints: For every policy, there can only
-- be one redeemer, and if there is a redeemer, there mus be some token names,
-- each with a non-zero amount of tokens.
type TxSkelMints =
  Map
    (Pl.Versioned Pl.MintingPolicy)
    (MintsRedeemer, NEMap Pl.TokenName (NonZero Integer))

-- | Combining 'TxSkelMints' in a sensible way. In particular, this means that
--
-- > Map.fromList [(pol, (red, NEMap.fromList [(tName, 1)]))]
--
-- and
--
-- > Map.fromList [(pol, (red, NEMap.fromList [(tName, -1)]))]
--
-- will combine to become the empty 'TxSkelMints' (and similar examples, where
-- the values add up to zero, see the comment at the definition of
-- 'addToTxSkelMints').
instance {-# OVERLAPPING #-} Semigroup TxSkelMints where
  a <> b =
    foldl
      (flip addToTxSkelMints)
      a
      (txSkelMintsToList b)

instance {-# OVERLAPPING #-} Monoid TxSkelMints where
  mempty = Map.empty

-- | Add a new entry to a 'TxSkelMints'. There are a few wrinkles:
--
-- (1) If for a given policy, redeemer, and token name, there are @n@ tokens in
-- the argument 'TxSkelMints', and you add @-n@ tokens, the corresponding entry
-- in the "inner map" of the policy will disappear (obviously, because all of
-- its values have to be non-zero). If that also means that the inner map
-- becomes empty, the policy will disappear from the 'TxSkelMints' altogether.
--
-- (2) If a policy is already present on the argument 'TxSkelMints' with a
-- redeemer @a@, and you add a mint with a different redeemer @b@, the old
-- redeemer is thrown away. This is because, on the plutus-apps 'Tx' type, every
-- minting policy can have only one redeemer per transaction. The values
-- associated with the token names of that policy are added as described above,
-- though. This means that any pre-exixting values will be minted with a new
-- redeemer.
--
-- If, for some reason, you really want to generate a 'TxSkelMints' that has
-- both a negative and a positive entry of the same asset class and redeemer,
-- you'll have to do so manually. Note, however, that even if you do so, NO
-- VALIDATOR OR MINTING POLICY WILL EVER GET TO SEE A TRANSACTION WITH SUCH
-- CONFLICTING INFORMATION. This is not a design decision/limitation of
-- cooked-validators: The Cardano API 'TxBodyContent' type, that we're
-- translating everything into eventually, stores minting information as a
-- minted value together with a map from policy IDs to witnesses (which
-- represent the used redeemers). That means that we can only store _one_
-- redeemer per minting policy, and no conflicting mints of the same asset
-- class, since they'll just cancel.
addToTxSkelMints ::
  (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) ->
  TxSkelMints ->
  TxSkelMints
addToTxSkelMints (pol, red, tName, NonZero amount) mints =
  case mints Map.!? pol of
    Nothing ->
      -- The policy isn't yet in the given 'TxSkelMints', so we can just add a
      -- new entry:
      Map.insert pol (red, NEMap.singleton tName (NonZero amount)) mints
    Just (_oldRed, innerMap) ->
      -- Ignore the old redeemer: If it's the same as the new one, nothing will
      -- change, if not, the new redeemer will be kept.
      case innerMap NEMap.!? tName of
        Nothing ->
          -- The given token name has not yet occurred for the given
          -- policy. This means that we can just add the new tokens to the
          -- inner map:
          Map.insert pol (red, NEMap.insert tName (NonZero amount) innerMap) mints
        Just (NonZero oldAmount) ->
          let newAmount = oldAmount + amount
           in if newAmount /= 0
                then -- If the sum of the old amount of tokens and the additional
                -- tokens is non-zero, we can just update the amount in the
                -- inner map:
                  Map.insert pol (red, NEMap.insert tName (NonZero newAmount) innerMap) mints
                else -- If the sum is zero, we'll have to delete the token name
                -- from the inner map. If that yields a completely empty
                -- inner map, we'll have to remove the entry altogether:
                case NEMap.nonEmptyMap $ NEMap.delete tName innerMap of
                  Nothing -> Map.delete pol mints
                  Just newInnerMap -> Map.insert pol (red, newInnerMap) mints

txSkelMintsToList :: TxSkelMints -> [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer)]
txSkelMintsToList =
  concatMap
    ( \(p, (r, m)) ->
        (\(t, n) -> (p, r, t, n))
          <$> NEList.toList (NEMap.toList m)
    )
    . Map.toList

-- | This function relies on the 'Monoid' instance of 'TxSkelMints'. So, some
-- non-empty lists (where all amounts for a given asset class an redeemer add up
-- to zero) might be translated into the empty 'TxSkelMints'. (See the comment
-- at the 'Semigroup' instance definition of 'TxSkelMints', and at the
-- definition of 'addToTxSkelMints'.)
txSkelMintsFromList :: [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer)] -> TxSkelMints
txSkelMintsFromList =
  foldMap
    ( \(policy, red, tName, amount) ->
        Map.singleton policy (red, NEMap.singleton tName amount)
    )

-- | Convert between 'TxSkelMints' and a list of tuples describing eveything
-- that's being minted. This is implemented in terms of 'txSkelMintsFromList'
-- (see the comment at that function). The upshot is that
--
-- > review mintsListIso . view mintsListIso
--
-- is the identity on 'TxSkelMints', but
--
-- > view mintsListIso . review mintsListIso
--
-- is NOT THE IDENTITY on @[(Pl.MintingPolicy, MintsRedeemer, Pl.TokenName,
-- NonZero Integer)]@.
mintsListIso :: Iso' TxSkelMints [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer)]
mintsListIso = iso txSkelMintsToList txSkelMintsFromList

-- | The value described by a 'TxSkelMints'
txSkelMintsValue :: TxSkelMints -> Pl.Value
txSkelMintsValue =
  foldMapOf
    (mintsListIso % folded)
    ( \(policy, _, tName, NonZero amount) ->
        Pl.assetClassValue
          ( Pl.assetClass
              (Pl.scriptCurrencySymbol policy)
              tName
          )
          amount
    )

-- * Transaction outputs

class IsTxSkelOutAllowedOwner a where
  toPKHOrValidator :: a -> Either Pl.PubKeyHash (Pl.Versioned Pl.Validator)

instance IsTxSkelOutAllowedOwner Pl.PubKeyHash where
  toPKHOrValidator = Left

instance IsTxSkelOutAllowedOwner (Pl.TypedValidator a) where
  toPKHOrValidator = Right . Pl.vValidatorScript

data TxSkelOut where
  Pays ::
    ( Show o, -- This is needed only for the 'Show' instance of 'TxSkel', which in turn is only needed in tests.
      IsOnchainOutput o,
      IsTxSkelOutAllowedOwner (OwnerType o),
      Typeable (OwnerType o),
      ToCredential (OwnerType o),
      DatumType o ~ TxSkelOutDatum, -- see the [note on TxSkelOutData]
      ValueType o ~ Pl.Value, -- needed for the 'txSkelOutValueL'
      ToScript (ReferenceScriptType o)
    ) =>
    {producedOutput :: o} ->
    TxSkelOut

-- | Since we mostly care about whether the transaction outputs are the same
-- on-chain, this is sufficient:
instance Eq TxSkelOut where
  (==) = (==) `on` txSkelOutToTxOut

deriving instance Show TxSkelOut

txSkelOutDatumL :: Lens' TxSkelOut TxSkelOutDatum
txSkelOutDatumL =
  lens
    (\(Pays output) -> output ^. outputDatumL)
    (\(Pays output) newDatum -> Pays $ output & outputDatumL .~ newDatum)

txSkelOutValueL :: Lens' TxSkelOut Pl.Value
txSkelOutValueL =
  lens
    (\(Pays output) -> outputValue output)
    (\(Pays output) newValue -> Pays $ output & outputValueL .~ newValue)

txSkelOutValue :: TxSkelOut -> Pl.Value
txSkelOutValue = (^. txSkelOutValueL)

txSkelOutValidator :: TxSkelOut -> Maybe (Pl.Versioned Pl.Validator)
txSkelOutValidator (Pays output) = rightToMaybe (toPKHOrValidator $ output ^. outputOwnerL)

type TxSkelOutDatumConstrs a = (Show a, PrettyCooked a, Pl.ToData a, Pl.Eq a, Typeable a)

-- | See the [note on TxSkelOut data]
data TxSkelOutDatum where
  -- | use no datum
  TxSkelOutNoDatum :: TxSkelOutDatum
  -- | only include the hash on the transaction
  TxSkelOutDatumHash :: TxSkelOutDatumConstrs a => a -> TxSkelOutDatum
  -- | use a 'Pl.OutputDatumHash' on the transaction output, but generate the
  -- transaction in such a way that the complete datum is included in the
  -- 'txInfoData' seen by validators
  TxSkelOutDatum :: TxSkelOutDatumConstrs a => a -> TxSkelOutDatum
  -- | use an inline datum
  TxSkelOutInlineDatum :: TxSkelOutDatumConstrs a => a -> TxSkelOutDatum

deriving instance Show TxSkelOutDatum

instance Eq TxSkelOutDatum where
  TxSkelOutNoDatum == TxSkelOutNoDatum = True
  TxSkelOutDatumHash datum1 == TxSkelOutDatumHash datum2 =
    case typeOf datum1 `eqTypeRep` typeOf datum2 of
      Just HRefl -> datum1 Pl.== datum2
      Nothing -> False
  TxSkelOutDatum datum1 == TxSkelOutDatum datum2 =
    case typeOf datum1 `eqTypeRep` typeOf datum2 of
      Just HRefl -> datum1 Pl.== datum2
      Nothing -> False
  TxSkelOutInlineDatum datum1 == TxSkelOutInlineDatum datum2 =
    case typeOf datum1 `eqTypeRep` typeOf datum2 of
      Just HRefl -> datum1 Pl.== datum2
      Nothing -> False
  _ == _ = False

instance Ord TxSkelOutDatum where
  compare x y = compare (txSkelOutUntypedDatum x) (txSkelOutUntypedDatum y)

-- | The 'PrettyCooked' instance for 'TxSkelOutDatum' relays the pretty-printing of
-- the datum it contains.
instance PrettyCooked TxSkelOutDatum where
  prettyCookedOpt _ TxSkelOutNoDatum = mempty
  prettyCookedOpt opts (TxSkelOutDatumHash datum) = prettyCookedOpt opts datum
  prettyCookedOpt opts (TxSkelOutDatum datum) = prettyCookedOpt opts datum
  prettyCookedOpt opts (TxSkelOutInlineDatum datum) = prettyCookedOpt opts datum

{- [note on TxSkelOut data]

On transaction outputs, we have the option to use

1. no datum
2. only a datum hash
3. a "normal" datum
4. an inline datum

These four options are also what the type 'TxSkelOutDatum' records. The
following table explains their differences.

\|                | in the simulated chain state | on the 'txInfoData' | 'Pl.OutputDatum' constructor seen by the validator |
\|----------------+------------------------------+---------------------+----------------------------------------------------|
\| no datum       | no                           | no                  | 'Pl.NoOutputDatum'                                 |
\|----------------+------------------------------+---------------------+----------------------------------------------------|
\| datum hash     | yes                          | no                  | 'Pl.OutputDatumHash'                               |
\|----------------+------------------------------+---------------------+----------------------------------------------------|
\| "normal" datum | yes                          | yes                 | 'Pl.OutputDatumHash'                               |
\|----------------+------------------------------+---------------------+----------------------------------------------------|
\| inline datum   | yes                          | no                  | 'Pl.OutputDatum'                                   |
\|----------------+------------------------------+---------------------+----------------------------------------------------|

That is:

- Whenever there is a datum, we'll store it in the state of our simulated
  chain. This will make it possible to retrieve it later, using functions such
  as 'datumFromHash'.

- Both of the 'TxSkelOutDatumHash' and 'TxSkelOutDatum' constructors will create
  an output that scripts see on the 'txInfo' as having a datum hash. The
  difference is whether that hash will be resolvable using validator functions
  like 'findDatum'.

In summary: On the one hand, there is the function 'txSkelOutDatumComplete'
which extracts the whole datum from a 'TxSkelOut'. On the other hand, there is
'txSkelOutToTxOut', which will return the output as seen on the 'txInfo' by a
validator, with the correct 'Pl.OutputDatum' on it.
-}

-- | The transaction output, as seen by a validator. In particular, see the
-- [note on TxSkelOut data].
txSkelOutToTxOut :: TxSkelOut -> Pl.TxOut
txSkelOutToTxOut (Pays output) = outputTxOut output

-- | See the [note on TxSkelOut data]
txSkelOutDatumComplete :: TxSkelOut -> Maybe Pl.Datum
txSkelOutDatumComplete (Pays output) = txSkelOutUntypedDatum $ output ^. outputDatumL

-- | See the [note on TxSkelOut data]
instance ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Pl.NoOutputDatum
  toOutputDatum (TxSkelOutDatumHash datum) = Pl.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
  toOutputDatum (TxSkelOutDatum datum) = Pl.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
  toOutputDatum (TxSkelOutInlineDatum datum) = Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData $ datum

txSkelOutUntypedDatum :: TxSkelOutDatum -> Maybe Pl.Datum
txSkelOutUntypedDatum = \case
  TxSkelOutNoDatum -> Nothing
  TxSkelOutDatumHash x -> Just (Pl.Datum $ Pl.toBuiltinData x)
  TxSkelOutDatum x -> Just (Pl.Datum $ Pl.toBuiltinData x)
  TxSkelOutInlineDatum x -> Just (Pl.Datum $ Pl.toBuiltinData x)

txSkelOutTypedDatum :: Pl.FromData a => TxSkelOutDatum -> Maybe a
txSkelOutTypedDatum = Pl.fromBuiltinData . Pl.getDatum <=< txSkelOutUntypedDatum

-- ** Smart constructors for transaction outputs

-- | Pay a certain value to a public key, without using a datum.
paysPK :: Pl.PubKeyHash -> Pl.Value -> TxSkelOut
paysPK pkh value =
  Pays
    ( ConcreteOutput
        pkh
        Nothing
        value
        TxSkelOutNoDatum
        (Nothing @(Pl.TypedValidator Pl.Any))
    )

-- | Pay a certain value to a public key, without a datum, but including a
-- reference script. This can be used to put reference scripts on chain.
paysPKWithReferenceScript :: Pl.PubKeyHash -> Pl.Value -> Pl.TypedValidator a -> TxSkelOut
paysPKWithReferenceScript pkh value refScript =
  Pays
    ( ConcreteOutput
        pkh
        Nothing
        value
        TxSkelOutNoDatum
        (Just refScript)
    )

-- | Pays a script a certain value with a certain datum, which will be included
-- as a datum hash on the transaction.
paysScript ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    PrettyCooked (Pl.DatumType a),
    Typeable a
  ) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Pl.Value ->
  TxSkelOut
paysScript validator datum value =
  Pays
    ( ConcreteOutput
        validator
        Nothing
        value
        (TxSkelOutDatum datum)
        (Nothing @(Pl.TypedValidator Pl.Any))
    )

-- | Like 'paysScript', but using an inline datum.
paysScriptInlineDatum ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    PrettyCooked (Pl.DatumType a),
    Typeable a
  ) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Pl.Value ->
  TxSkelOut
paysScriptInlineDatum validator datum value =
  Pays
    ( ConcreteOutput
        validator
        Nothing
        value
        (TxSkelOutInlineDatum datum)
        (Nothing @(Pl.TypedValidator Pl.Any))
    )

-- | Like 'paysScript', but won't include the complete datum on the
-- transaction. This is only useful if there's no script that checks the output
-- datum.
paysScriptDatumHash ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    PrettyCooked (Pl.DatumType a),
    Typeable a
  ) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Pl.Value ->
  TxSkelOut
paysScriptDatumHash validator datum value =
  Pays
    ( ConcreteOutput
        validator
        Nothing
        value
        (TxSkelOutDatumHash datum)
        (Nothing @(Pl.TypedValidator Pl.Any))
    )

-- * Redeemers for transaction inputs

type SpendsScriptConstrs redeemer =
  ( Pl.ToData redeemer,
    Show redeemer,
    PrettyCooked redeemer,
    Pl.Eq redeemer,
    Typeable redeemer
  )

data TxSkelRedeemer where
  TxSkelNoRedeemerForPK :: TxSkelRedeemer
  TxSkelRedeemerForScript :: SpendsScriptConstrs redeemer => redeemer -> TxSkelRedeemer
  TxSkelRedeemerForReferencedScript :: SpendsScriptConstrs redeemer => redeemer -> TxSkelRedeemer

txSkelTypedRedeemer :: Pl.FromData (Pl.RedeemerType a) => TxSkelRedeemer -> Maybe (Pl.RedeemerType a)
txSkelTypedRedeemer (TxSkelRedeemerForScript redeemer) = Pl.fromData . Pl.toData $ redeemer
txSkelTypedRedeemer (TxSkelRedeemerForReferencedScript redeemer) = Pl.fromData . Pl.toData $ redeemer
txSkelTypedRedeemer _ = Nothing

deriving instance (Show TxSkelRedeemer)

instance Eq TxSkelRedeemer where
  TxSkelNoRedeemerForPK == TxSkelNoRedeemerForPK = True
  (TxSkelRedeemerForScript r1) == (TxSkelRedeemerForScript r2) =
    case typeOf r1 `eqTypeRep` typeOf r2 of
      Just HRefl -> r1 Pl.== r2
      Nothing -> False
  (TxSkelRedeemerForReferencedScript r1) == (TxSkelRedeemerForReferencedScript r2) =
    TxSkelRedeemerForScript r1 == TxSkelRedeemerForScript r2
  _ == _ = False

-- * Transaction skeletons

data TxSkel where
  TxSkel ::
    { txSkelLabel :: Set TxLabel,
      txSkelOpts :: TxOpts,
      txSkelMints :: TxSkelMints,
      txSkelValidityRange :: Pl.POSIXTimeRange,
      txSkelSigners :: NonEmpty Wallet,
      txSkelIns :: Map Pl.TxOutRef TxSkelRedeemer,
      txSkelInsReference :: Set Pl.TxOutRef,
      txSkelOuts :: [TxSkelOut]
    } ->
    TxSkel
  deriving (Show, Eq)

makeLensesFor
  [ ("txSkelLabel", "txSkelLabelL"),
    ("txSkelOpts", "txSkelOptsL"),
    ("txSkelMints", "txSkelMintsL"),
    ("txSkelValidityRange", "txSkelValidityRangeL"),
    ("txSkelSigners", "txSkelSignersL"),
    ("txSkelIns", "txSkelInsL"),
    ("txSkelInsReference", "txSkelInsReferenceL"),
    ("txSkelInsCollateral", "txSkelInsCollateralL"),
    ("txSkelOuts", "txSkelOutsL"),
    ("txSkelFee", "txSkelFeeL")
  ]
  ''TxSkel

-- | A convenience template where wallet 1 is the default signer of an
-- otherwise empty transaction skeleton.
txSkelTemplate :: TxSkel
txSkelTemplate =
  TxSkel
    { txSkelLabel = Set.empty,
      txSkelOpts = def,
      txSkelMints = Map.empty,
      txSkelValidityRange = Pl.always,
      txSkelSigners = wallet 1 NEList.:| [],
      txSkelIns = Map.empty,
      txSkelInsReference = Set.empty,
      txSkelOuts = []
    }

-- | Return all data on transaction outputs.
txSkelOutputData :: TxSkel -> Map Pl.DatumHash TxSkelOutDatum
txSkelOutputData =
  foldMapOf
    ( txSkelOutsL
        % folded
        % txSkelOutDatumL
    )
    ( \txSkelOutDatum -> do
        maybe
          Map.empty
          (\datum -> Map.singleton (Pl.datumHash datum) txSkelOutDatum)
          (txSkelOutUntypedDatum txSkelOutDatum)
    )

-- Map.singleton (Pl.datumHash <$> txSkelOutUntypedDatum datum) datum)

newtype Fee = Fee {feeLovelace :: Integer} deriving (Eq, Ord, Show, Num)

-- | The value in all transaction inputs, plus the negative parts of the minted
-- value. This is the right hand side of the "balancing equation":
--
-- > mints + inputs = fees + burns + outputs
txSkelOutputValue :: TxSkel -> Fee -> Pl.Value
txSkelOutputValue skel@TxSkel {txSkelMints = mints} fees =
  negativePart (txSkelMintsValue mints)
    <> foldOf (txSkelOutsL % folded % txSkelOutValueL) skel
    <> Pl.lovelaceValueOf (feeLovelace fees)

txSkelOutValidators :: TxSkel -> Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
txSkelOutValidators =
  Map.fromList
    . mapMaybe (fmap (\script -> (Ledger.Scripts.validatorHash script, script)) . txSkelOutValidator)
    . txSkelOuts

-- -- | All of the '_txSkelRequiredSigners', plus all of the signers required for
-- -- PK inputs on the transaction
-- txSkelAllSigners :: TxSkel -> Set Pl.PubKeyHash
-- txSkelAllSigners skel =
--   (skel ^. txSkelRequiredSignersL)
--     <> foldMapOf
--       (consumedOutputsF % afolding sBelongsToPubKey)
--       Set.singleton
--       skel

-- * Utilities

-- ** Working with 'Value's

flattenValueI :: Iso' Pl.Value [(Pl.AssetClass, Integer)]
flattenValueI =
  iso
    (map (\(cSymbol, tName, amount) -> (Pl.assetClass cSymbol tName, amount)) . Pl.flattenValue)
    (foldl' (\v (ac, amount) -> v <> Pl.assetClassValue ac amount) mempty)

-- | The positive part of a value. For every asset class in the given value,
-- this asset class and its amount are included in the output iff the amount is
-- strictly positive. It holds
--
-- > x == positivePart x <> Pl.negate negativePart x
positivePart :: Pl.Value -> Pl.Value
positivePart = over flattenValueI (filter $ (0 <) . snd)

-- | The negative part of a value. For every asset class in the given value,
-- this asset class and its negated amount are included in the output iff the
-- amount is strictly negative. It holds
--
-- > x == positivePart x <> Pl.negate negativePart x
negativePart :: Pl.Value -> Pl.Value
negativePart = positivePart . Pl.negate

-- | Focus the Ada part in a value. This is useful if you want to chcange only
-- that part.
adaL :: Lens' Pl.Value Pl.Ada
adaL =
  lens
    Pl.fromValue
    ( \value (Pl.Lovelace ada) ->
        over
          flattenValueI
          (\l -> insertAssocList l (Pl.assetClass Pl.adaSymbol Pl.adaToken) ada)
          value
    )
  where
    insertAssocList :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
    insertAssocList l a b = (a, b) : filter ((/= a) . fst) l

-- Various Optics on 'TxSkels' and all the other types defined in
-- 'Cooked.Tx.Constraints.Type'.

-- | Decide if a transaction output has a certain owner and datum type.
txSkelOutOwnerTypeP ::
  forall ownerType.
  ( ToCredential ownerType,
    Show ownerType,
    IsTxSkelOutAllowedOwner ownerType,
    Typeable ownerType
  ) =>
  Prism' TxSkelOut (ConcreteOutput ownerType TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script))
txSkelOutOwnerTypeP =
  prism'
    Pays
    ( \(Pays output) ->
        let owner = output ^. outputOwnerL
         in case typeOf owner `eqTypeRep` typeRep @ownerType of
              Just HRefl ->
                Just $
                  ConcreteOutput
                    owner
                    (output ^. outputStakingCredentialL)
                    (output ^. outputValueL)
                    (output ^. outputDatumL)
                    (toScript <$> output ^. outputReferenceScriptL)
              Nothing -> Nothing
    )

txSkelOutputDatumTypeAT ::
  (Pl.FromData a, Typeable a) =>
  AffineTraversal' TxSkelOut a
txSkelOutputDatumTypeAT =
  atraversal
    ( \txSkelOut -> case txSkelOutDatumComplete txSkelOut of
        Nothing -> Left txSkelOut
        Just (Pl.Datum datum) -> case Pl.fromBuiltinData datum of
          Just tyDatum -> Right tyDatum
          Nothing -> Left txSkelOut
    )
    ( \(Pays output) newTyDatum ->
        Pays $
          over
            outputDatumL
            ( \case
                TxSkelOutNoDatum -> TxSkelOutNoDatum
                TxSkelOutDatum tyDatum -> TxSkelOutDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutDatumHash tyDatum -> TxSkelOutDatumHash $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutInlineDatum tyDatum -> TxSkelOutInlineDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
            )
            output
    )
  where
    replaceDatumOnCorrectType :: (Typeable b, Typeable a) => b -> a -> b
    replaceDatumOnCorrectType old new = case typeOf old `eqTypeRep` typeOf new of
      Just HRefl -> new
      Nothing -> old

data SkelContext = SkelContext
  { skelContextTxOuts :: Map Pl.TxOutRef Pl.TxOut,
    skelContextTxSkelOutDatums :: Map Pl.DatumHash TxSkelOutDatum
  }
