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

module Cooked.Skeleton
  ( LabelConstrs,
    TxLabel (..),
    BalanceOutputPolicy (..),
    BalancingWallet (..),
    RawModTx (..),
    applyRawModOnBalancedTx,
    TxOpts (..),
    MintsConstrs,
    MintsRedeemer (..),
    TxSkelMints,
    addToTxSkelMints,
    txSkelMintsToList,
    txSkelMintsFromList,
    txSkelMintsValue,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutValue,
    txSkelOutValidator,
    TxSkelOutDatumConstrs,
    TxSkelOutDatum (..),
    TxSkelOut (..),
    paysPK,
    paysPKWithReferenceScript,
    txSkelOutTypedDatum,
    txSkelOutUntypedDatum,
    paysScript,
    paysScriptInlineDatum,
    paysScriptDatumHash,
    TxSkelRedeemer (..),
    txSkelTypedRedeemer,
    TxSkel (..),
    txSkelLabelL,
    txSkelOptsL,
    txSkelMintsL,
    txSkelValidityRangeL,
    txSkelSignersL,
    txSkelInsL,
    txSkelInsReferenceL,
    txSkelOutsL,
    txSkelTemplate,
    txSkelOutputData,
    Fee (..),
    txSkelOutputValue,
    txSkelOutValidators,
    txSkelOutOwnerTypeP,
    txSkelOutputDatumTypeAT,
    SkelContext (..),
    txSkelOutReferenceScripts,
  )
where

import qualified Cardano.Api as C
import Control.Monad
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Default
import Data.Either.Combinators
import Data.Function
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
import qualified Ledger.Slot as Pl
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

-- * Transaction labels

type LabelConstrs x = (Show x, Typeable x, Eq x, Ord x)

data TxLabel where
  TxLabel :: LabelConstrs x => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

instance Show TxLabel where
  show (TxLabel x) = show x

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

-- | Whether to adjust a potentially existing output to the balancing wallet
-- with the change during transaction balancing.
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
    -- final signing are performed
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

-- | Set of options to modify the behavior of generating and validating some transaction.
data TxOpts = TxOpts
  { -- | Performs an adjustment to unbalanced transactions, making sure every
    -- UTxO that is produced has the necessary minimum amount of Ada.
    --
    -- Default is @False@.
    txOptEnsureMinAda :: Bool,
    -- | Ignore this for now. Deprecated.
    txOptAwaitTxConfirmed :: Bool,
    -- | Whether to increase the slot counter automatically on transaction
    -- submission.  This is useful for modelling transactions that could be
    -- submitted in parallel in reality, so there should be no explicit ordering
    -- of what comes first.
    --
    -- Default is @True@.
    txOptAutoSlotIncrease :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted ('txOptEnsureMinAda') and balanced. The name of this
    -- option contains /unsafe/ to draw attention to the fact that modifying a
    -- transaction at that stage might make it invalid. Still, this offers a
    -- hook for being able to alter a transaction in unforeseen ways. It is
    -- mostly used to test contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with
    --
    -- > txOptUnsafeModTx = [RawModTxAfterBalancing Debug.Trace.traceShowId]
    --
    -- The leftmost function in the list is applied first.
    --
    -- Default is @[]@.
    txOptUnsafeModTx :: [RawModTx],
    -- | Whether to balance the transaction or not. Balancing ensures that
    --
    -- > input + mints == output + fees + burns
    --
    -- If you decide to set @txOptBalance = False@ you will have trouble
    -- satisfying that equation by hand because @fees@ are variable. You will
    -- likely see a error about value preservation, and should adjust the fees
    -- accordingly.
    --
    -- Default is @True@, and nobody in their right mind will ever set it
    -- otherwise.
    txOptBalance :: Bool,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- Default is 'AdjustExistingOutput'.
    txOptBalanceOutputPolicy :: BalanceOutputPolicy,
    -- | Which wallet to use to provide outputs for balancing and collaterals.
    -- Either the first signer by default, or an explicit wallet. In the second
    -- case, this wallet must be a signer of the transaction. This option WILL
    -- NOT ensure that it is added in case it is not already present in the list
    -- of signers.
    --
    -- Default is 'BalanceWithFirstSigner'.
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

-- | Which redeemer to use for minting. Note that using 'NoMintsRedeemer'
-- corresponds to the redeemer @()@ on-chain.
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
  compare NoMintsRedeemer SomeMintsRedeemer {} = LT
  compare SomeMintsRedeemer {} NoMintsRedeemer = GT
  compare (SomeMintsRedeemer a) (SomeMintsRedeemer b) =
    case compare (SomeTypeRep $ typeOf a) (SomeTypeRep $ typeOf b) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf b of
        Just HRefl -> compare (Pl.toData a) (Pl.toData b)
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"

-- | A description of what a transaction mints. For every policy, there can only
-- be one 'MintsRedeemer', and if there is, there must be some token names, each
-- with a non-zero amount of tokens.
--
-- You'll probably not construct this by hand, but use 'txSkelMintsFromList'.
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
-- > Map.fromList [(pol, (red', NEMap.fromList [(tName, -1)]))]
--
-- will combine to become the empty 'TxSkelMints' (and similar examples, where
-- the values add up to zero, see the comment at the definition of
-- 'addToTxSkelMints').
--
-- In every case, if you add mints with a different redeemer for the same
-- policy, the redeemer used in the right argument takes precedence.
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
-- redeemer is thrown away. The values associated with the token names of that
-- policy are added as described above, though. This means that any pre-existing
-- values will be minted with a new redeemer.
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
  (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer) ->
  TxSkelMints ->
  TxSkelMints
addToTxSkelMints (pol, red, tName, amount) mints
  | 0 == amount = mints
  | otherwise = case mints Map.!? pol of
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

-- | Convert from 'TxSkelMints' to a list of tuples describing eveything that's
-- being minted.
txSkelMintsToList :: TxSkelMints -> [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer)]
txSkelMintsToList =
  concatMap
    ( \(p, (r, m)) ->
        (\(t, NonZero n) -> (p, r, t, n))
          <$> NEList.toList (NEMap.toList m)
    )
    . Map.toList

-- | Smart constructor for 'TxSkelMints'. This function relies on
-- 'addToTxSkelMints'. So, some non-empty lists (where all amounts for a given
-- asset class an redeemer add up to zero) might be translated into the empty
-- 'TxSkelMints'.
txSkelMintsFromList :: [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer)] -> TxSkelMints
txSkelMintsFromList = foldr addToTxSkelMints mempty

-- | The value described by a 'TxSkelMints'
txSkelMintsValue :: TxSkelMints -> Pl.Value
txSkelMintsValue =
  foldMapOf
    (to txSkelMintsToList % folded)
    ( \(policy, _, tName, amount) ->
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

-- | Transaction outputs. The 'Pays' constructor is really general, and you'll
-- probably want to use one of the smart constructors like 'paysScript' or
-- 'paysPK' in most cases.
data TxSkelOut where
  Pays ::
    ( Show o, -- This is needed only for the 'Show' instance of 'TxSkel', which in turn is only needed in tests.
      Typeable o,
      IsTxInfoOutput o,
      IsTxSkelOutAllowedOwner (OwnerType o),
      Typeable (OwnerType o),
      ToCredential (OwnerType o),
      DatumType o ~ TxSkelOutDatum,
      ValueType o ~ Pl.Value, -- needed for the 'txSkelOutValueL'
      ToScript (ReferenceScriptType o)
    ) =>
    {producedOutput :: o} ->
    TxSkelOut

instance Eq TxSkelOut where
  Pays a == Pays b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl -> outputTxOut a == outputTxOut b
    Nothing -> False

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

-- | On transaction outputs, we have the options to use
--
-- 1. no datum
-- 2. only a datum hash
-- 3. a "normal" datum
-- 4. an inline datum
--
-- These four options are also what the type 'TxSkelOutDatum' records. The
-- following table explains their differences.
--
-- +------------------------+------------------+---------------------+-----------------------+
-- |                        | datum stored in  |                     | 'Pl.OutputDatum'      |
-- |                        | in the simulated | datum resolved      | constructor           |
-- |                        | chain state      | on the 'txInfoData' | seen by the validator |
-- +========================+==================+=====================+=======================+
-- | 'TxSkelOutNoDatum'     | no               | no                  | 'Pl.NoOutputDatum'    |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatumHash'   | yes              | no                  | 'Pl.OutputDatumHash'  |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatum'       | yes              | yes                 | 'Pl.OutputDatumHash'  |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutInlineDatum' | yes              | no                  | 'Pl.OutputDatum'      |
-- +------------------------+------------------+---------------------+-----------------------+
--
-- That is:
--
-- - Whenever there is a datum, we'll store it in the state of our simulated
--   chain. This will make it possible to retrieve it later, using functions such
--   as 'datumFromHash'.
--
-- - Both of the 'TxSkelOutDatumHash' and 'TxSkelOutDatum' constructors will create
--   an output that scripts see on the 'txInfo' as having a datum hash. The
--   difference is whether that hash will be resolvable using validator functions
--   like 'findDatum'.
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
  x == y = compare x y == EQ

instance Ord TxSkelOutDatum where
  compare TxSkelOutNoDatum TxSkelOutNoDatum = EQ
  compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2) =
    case compare (SomeTypeRep (typeOf d1)) (SomeTypeRep (typeOf d2)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf d1 `eqTypeRep` typeOf d2 of
        Just HRefl -> compare (Pl.toBuiltinData d1) (Pl.toBuiltinData d2)
        Nothing -> error "This branch cannot happen: un-equal type representations that compare to EQ"
  compare (TxSkelOutDatum d1) (TxSkelOutDatum d2) =
    compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2)
  compare (TxSkelOutInlineDatum d1) (TxSkelOutInlineDatum d2) =
    compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2)
  compare TxSkelOutDatumHash {} TxSkelOutNoDatum = GT
  compare TxSkelOutDatum {} TxSkelOutNoDatum = GT
  compare TxSkelOutDatum {} TxSkelOutDatumHash {} = GT
  compare TxSkelOutInlineDatum {} _ = GT
  compare _ _ = LT

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

-- | Pay a certain value to a public key.
paysPK :: Pl.PubKeyHash -> Pl.Value -> TxSkelOut
paysPK pkh value =
  Pays
    ( ConcreteOutput
        pkh
        Nothing
        value
        TxSkelOutNoDatum
        (Nothing @(Pl.Versioned Pl.Script))
    )

-- | Pay a certain value to a public key, including a reference script. This can
-- be used to put reference scripts on chain.
paysPKWithReferenceScript :: Typeable a => Pl.PubKeyHash -> Pl.Value -> Pl.TypedValidator a -> TxSkelOut
paysPKWithReferenceScript pkh value refScript =
  Pays
    ( ConcreteOutput
        pkh
        Nothing
        value
        TxSkelOutNoDatum
        (Just refScript)
    )

-- | Pays a script a certain value with a certain datum, using the
-- 'TxSkelOutDatum' constructor. (See the documentation of 'TxSkelOutDatum'.)
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
        (Nothing @(Pl.Versioned Pl.Script))
    )

-- | Like 'paysScript', but using the 'TxSkelOutInlineDatum' constructor for the
-- datum.
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
        (Nothing @(Pl.Versioned Pl.Script))
    )

-- | Like 'paysScript', but using the 'TxSkelOutDatumHash' constructor. This is
-- only useful if there's no script that checks the output datum.
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
        (Nothing @(Pl.Versioned Pl.Script))
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
    { -- | Labels do not influence the transaction generation at all; they are
      -- pretty-printed whenever cooked-validators prints a transaction, and can
      -- therefore make the output more informative (and greppable).
      txSkelLabel :: Set TxLabel,
      -- | Some options that control transaction generation.
      txSkelOpts :: TxOpts,
      -- | Any value minted or burned by the transaction. You'll probably want
      -- to use 'txSkelMintsFromList' to construct this.
      txSkelMints :: TxSkelMints,
      -- | The wallets signing the transaction. This list must contain at least
      -- one element. By default, the first signer will pay for fees and
      -- balancing. You can change that with 'txOptBalanceWallet'.
      txSkelSigners :: [Wallet],
      txSkelValidityRange :: Pl.SlotRange,
      -- | To each 'TxOutRef' the transaction should consume, add a redeemer
      -- specifying how to spend it. You must make sure that
      --
      -- - On 'TxOutRef's referencing UTxOs belonging to public keys, you use
      --   the 'TxSkelNoRedeemerForPK' constructor.
      --
      -- - On 'TxOutRef's referencing UTxOs belonging to scripts, you must make
      --   sure that the type of the redeemer is appropriate for the script.
      --
      -- - On 'TxOutRef's belonging to /referenced/ scripts, you must make sure
      --   that the UTxO where the referenced script is stored is included in
      --   the 'txSkelInsReference'.
      txSkelIns :: Map Pl.TxOutRef TxSkelRedeemer,
      -- | All outputs referenced by the transaction.
      txSkelInsReference :: Set Pl.TxOutRef,
      -- | The outputs of the transaction. These will occur in exactly this
      -- order on the transaction.
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

-- | A convenience template of an empty transaction skeleton.
txSkelTemplate :: TxSkel
txSkelTemplate =
  TxSkel
    { txSkelLabel = Set.empty,
      txSkelOpts = def,
      txSkelMints = Map.empty,
      txSkelValidityRange = Pl.always,
      txSkelSigners = [],
      txSkelIns = Map.empty,
      txSkelInsReference = Set.empty,
      txSkelOuts = []
    }

-- | The missing information on a 'TxSkel' that can only be resolved by querying
-- the state of the blockchain.
data SkelContext = SkelContext
  { skelContextTxOuts :: Map Pl.TxOutRef Pl.TxOut,
    skelContextTxSkelOutDatums :: Map Pl.DatumHash TxSkelOutDatum
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

-- | All validators which will receive transaction outputs
txSkelOutValidators :: TxSkel -> Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
txSkelOutValidators =
  Map.fromList
    . mapMaybe (fmap (\script -> (Ledger.Scripts.validatorHash script, script)) . txSkelOutValidator)
    . txSkelOuts

-- | All validators in the reference script field of transaction outputs
txSkelOutReferenceScripts :: TxSkel -> Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
txSkelOutReferenceScripts =
  mconcat
    . map
      ( \(Pays output) ->
          case output ^. outputReferenceScriptL of
            Nothing -> Map.empty
            Just x ->
              let vScript@(Pl.Versioned script version) = toScript x
                  Pl.ScriptHash hash = toScriptHash vScript
               in Map.singleton (Pl.ValidatorHash hash) $ Pl.Versioned (Pl.Validator script) version
      )
    . txSkelOuts

-- * Various Optics on 'TxSkels' and all the other types defined here

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

    txSkelOutDatumComplete :: TxSkelOut -> Maybe Pl.Datum
    txSkelOutDatumComplete (Pays output) = txSkelOutUntypedDatum $ output ^. outputDatumL
