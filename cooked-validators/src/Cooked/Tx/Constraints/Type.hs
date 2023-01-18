{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Tx.Constraints.Type where

import qualified Cardano.Api as C
import Control.Monad
import Cooked.MockChain.Wallet
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
import qualified Ledger.Ada as Pl
import qualified Ledger.Scripts (validatorHash)
import qualified Ledger.Scripts as Pl hiding (validatorHash)
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import Optics.TH
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V2.Ledger.Api as Pl hiding (TxOut)
import qualified Plutus.V2.Ledger.Tx as Pl
import qualified PlutusTx.Prelude as Pl
import Prettyprinter (Doc, Pretty)
import qualified Prettyprinter as PP
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

-- * 'IsOutput': UTxOs that can be used as transaction outputs

-- | A generalisation of 'Pl.TxOut': With the two type families, we can lift
-- some information about who owns the output (a public key, a script...?) and
-- the datum (do we have an inline datum, a datum hash, nothing...?) to the type
-- level.
class Show o => IsOutput o where
  -- The owner type can be, in particular, a 'TypedValidator a' or a
  -- 'PubkeyHash'
  type OwnerType o
  type DatumType o
  type ValueType o
  outputOwnerL :: Lens' o (OwnerType o)
  outputStakingCredentialL :: Lens' o (Maybe Pl.StakingCredential)
  outputDatumL :: Lens' o (DatumType o)
  outputValueL :: Lens' o (ValueType o)
  outputAddress :: o -> Pl.Address
  outputOutputDatum :: o -> Pl.OutputDatum
  outputValue :: o -> Pl.Value

-- | Return the output as it is seen by a validator. In particular the
-- correctness of this specification will depend on the 'IsOutput' instance, so
-- make sure you get these instances (in particular the functions 'outputAddress',
-- 'outputOutputDatum', and 'outputValue') right!
outputTxOut :: IsOutput o => o -> Pl.TxOut
outputTxOut o =
  Pl.TxOut
    (outputAddress o)
    (outputValue o)
    (outputOutputDatum o)
    Nothing -- TODO for when we introduce reference scripts

-- ** 'Pl.TxOut's are outputs

instance IsOutput Pl.TxOut where
  type OwnerType Pl.TxOut = Pl.Credential
  type DatumType Pl.TxOut = Pl.OutputDatum
  type ValueType Pl.TxOut = Pl.Value
  outputOwnerL =
    lensVL Pl.outAddress
      % lens
        Pl.addressCredential
        (\addr cred -> addr {Pl.addressCredential = cred})
  outputDatumL = lensVL Pl.outDatum
  outputStakingCredentialL =
    lens
      (Pl.addressStakingCredential . Pl.txOutAddress)
      ( \out mStCred ->
          out {Pl.txOutAddress = (Pl.txOutAddress out) {Pl.addressStakingCredential = mStCred}}
      )
  outputValueL = lensVL Pl.outValue
  outputAddress = Pl.txOutAddress
  outputOutputDatum = Pl.txOutDatum
  outputValue = Pl.txOutValue

-- ** A concrete type for outputs

class ToCredential a where
  toCredential :: a -> Pl.Credential

instance ToCredential Pl.Credential where
  toCredential = id

instance ToCredential (Pl.TypedValidator a) where
  toCredential = Pl.ScriptCredential . Pl.validatorHash

instance ToCredential Pl.PubKeyHash where
  toCredential = Pl.PubKeyCredential

class ToOutputDatum a where
  toOutputDatum :: a -> Pl.OutputDatum

instance ToOutputDatum Pl.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum Pl.Datum where
  toOutputDatum = Pl.OutputDatum

instance ToOutputDatum () where
  toOutputDatum = const Pl.NoOutputDatum

instance ToOutputDatum Pl.DatumHash where
  toOutputDatum = Pl.OutputDatumHash

instance Pl.ToData a => ToOutputDatum (ResolvedOrInlineDatum a) where
  toOutputDatum (ResolvedOrInlineDatum x) = Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData $ x

class ToValue a where
  toValue :: a -> Pl.Value

instance ToValue Pl.Value where
  toValue = id

instance ToValue Pl.Ada where
  toValue = Pl.toValue

data ConcreteOutput ownerType datumType valueType where
  ConcreteOutput ::
    { concreteOutputOwner :: ownerType,
      concreteOutputStakingCredential :: Maybe Pl.StakingCredential,
      concreteOutputValue :: valueType,
      concreteOutputDatum :: datumType
      -- concreteOutputReferenceScript :: Maybe Pl.ScriptHash -- TODO for when we introduce reference scripts
    } ->
    ConcreteOutput ownerType datumType valueType

deriving instance (Show ownerType, Show datumType, Show valueType) => Show (ConcreteOutput ownerType datumType valueType)

deriving instance (Eq ownerType, Eq datumType, Eq valueType) => Eq (ConcreteOutput ownerType datumType valueType)

instance
  ( Show ownerType,
    ToCredential ownerType,
    Show datumType,
    ToOutputDatum datumType,
    Show valueType,
    ToValue valueType
  ) =>
  IsOutput (ConcreteOutput ownerType datumType valueType)
  where
  type OwnerType (ConcreteOutput ownerType datumType valueType) = ownerType
  type DatumType (ConcreteOutput ownerType datumType valueType) = datumType
  type ValueType (ConcreteOutput ownerType datumType valueType) = valueType
  outputOwnerL = lens concreteOutputOwner (\out owner -> out {concreteOutputOwner = owner})
  outputStakingCredentialL = lens concreteOutputStakingCredential (\out mStCred -> out {concreteOutputStakingCredential = mStCred})
  outputDatumL = lens concreteOutputDatum (\out datum -> out {concreteOutputDatum = datum})
  outputValueL = lens concreteOutputValue (\out value -> out {concreteOutputValue = value})
  outputAddress out =
    Pl.Address
      (toCredential $ concreteOutputOwner out)
      (concreteOutputStakingCredential out)
  outputOutputDatum = toOutputDatum . concreteOutputDatum
  outputValue = toValue . concreteOutputValue

-- ** A few special concrete outputs

-- | A public key output without a datum
type PKOutput = ConcreteOutput Pl.PubKeyHash () Pl.Value

-- | A public key output that only has Ada and no datum
type PKAdaOnlyOutput = ConcreteOutput Pl.PubKeyHash () Pl.Ada

-- | A public key output where we don't know anything about the datum: It is a
-- general 'Pl.OutputDatum'
type PKOutputMaybeDatum = ConcreteOutput Pl.PubKeyHash Pl.OutputDatum Pl.Value

-- | An output that belongs to a typed validator and has an inline datum of the
-- appropriate type.
type ScriptOutputWithInlineDatum a = ConcreteOutput (Pl.TypedValidator a) (Pl.DatumType a) Pl.Value

-- TODO et cetera

-- ** Functions to translate between different output types

-- | Test if there is no datum on an output. If there is no datum, return an
-- output with the same 'OwnerType', but with @()@ as its 'DatumType'.
isOutputWithoutDatum ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) () (ValueType output))
isOutputWithoutDatum out = case outputOutputDatum out of
  Pl.NoOutputDatum ->
    Just $
      ConcreteOutput
        (out ^. outputOwnerL)
        (out ^. outputStakingCredentialL)
        (out ^. outputValueL)
        ()
  _ -> Nothing

-- ** Functions to translate between different output types

-- | Test if the output carries some inlined datum (lose the type information
-- about the datum in favour of Plutus' 'Datum' type).
isOutputWithInlineDatumUntyped ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.Datum (ValueType output))
isOutputWithInlineDatumUntyped out =
  case outputOutputDatum out of
    Pl.OutputDatum datum ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (out ^. outputValueL)
          datum
    _ -> Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum ::
  IsOutput output =>
  output ->
  Maybe output
isOutputWithInlineDatum out =
  case outputOutputDatum out of
    Pl.OutputDatum _ -> Just out
    _ -> Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.DatumHash (ValueType output))
isOutputWithDatumHash out =
  case outputOutputDatum out of
    Pl.OutputDatumHash hash ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (Pl.addressStakingCredential . outputAddress $ out)
          (out ^. outputValueL)
          hash
    _ -> Nothing

-- | Test if the value carried by an output verifies a given predicate.
isOutputWithValueSuchThat ::
  IsOutput output =>
  (ValueType output -> Bool) ->
  output ->
  Maybe output
isOutputWithValueSuchThat predicate out
  | predicate (out ^. outputValueL) = Just out
  | otherwise = Nothing

-- | Test if the datum carried by an output verifies a given predicate.
isOutputWithDatumSuchThat ::
  IsOutput output =>
  (DatumType output -> Bool) ->
  output ->
  Maybe output
isOutputWithDatumSuchThat predicate out
  | predicate (out ^. outputDatumL) = Just out
  | otherwise = Nothing

-- | Wrapper type to make clear that the datum is a resolved or inline datum
newtype ResolvedOrInlineDatum a = ResolvedOrInlineDatum a deriving (Show, Eq)

-- | Test if an output has an inline datum of a certain type. In most
-- applications, it will make sense to use this with 'filteredUtxosWithDatums',
-- and not with 'filteredUtxos'.
isOutputWithDatumOfType ::
  forall a output.
  ( Pl.FromData a,
    IsOutput output
  ) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (ResolvedOrInlineDatum a) (ValueType output))
isOutputWithDatumOfType out = case outputOutputDatum out of
  Pl.OutputDatumHash _ -> Nothing
  Pl.OutputDatum (Pl.Datum datum) ->
    ConcreteOutput
      (out ^. outputOwnerL)
      (out ^. outputStakingCredentialL)
      (out ^. outputValueL)
      . ResolvedOrInlineDatum
      <$> Pl.fromBuiltinData datum
  Pl.NoOutputDatum -> Nothing

-- | Test if the owner an output is a specific script. If it is, return an
-- output with the validator type as its 'OwnerType'.
isScriptOutputFrom ::
  IsOutput output =>
  Pl.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Pl.TypedValidator a) (DatumType output) (ValueType output))
isScriptOutputFrom validator out =
  case outputAddress out of
    Pl.Address (Pl.ScriptCredential scriptHash) mStCred ->
      if scriptHash == Pl.validatorHash validator
        then
          Just $
            ConcreteOutput
              validator
              mStCred
              (out ^. outputValueL)
              (out ^. outputDatumL)
        else Nothing
    _ -> Nothing

-- | like 'isScriptOutputFrom', but also makes sure that the output has an
-- inline datum of the correct type. In most applications, it will make sense to
-- use this with 'filteredUtxosWithDatums', and not with 'filteredUtxos'.
isScriptOutputFrom' ::
  forall a output.
  ( IsOutput output,
    Pl.FromData (Pl.DatumType a),
    ToOutputDatum (DatumType output),
    Show (DatumType output),
    ToValue (ValueType output),
    Show (ValueType output)
  ) =>
  Pl.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Pl.TypedValidator a) (ResolvedOrInlineDatum (Pl.DatumType a)) (ValueType output))
isScriptOutputFrom' validator =
  -- what's all this madness witht the type annotations?
  isScriptOutputFrom @output @a validator
    >=> isOutputWithDatumOfType @(Pl.DatumType a) @(ConcreteOutput (Pl.TypedValidator a) (DatumType output) (ValueType output))

-- | Test if the owner an output is a specific public key. If it is, return an
-- output of the same 'DatumType', but with 'Pl.PubKeyHash' as its 'OwnerType'.
isPKOutputFrom ::
  IsOutput output =>
  Pl.PubKeyHash ->
  output ->
  Maybe (ConcreteOutput Pl.PubKeyHash (DatumType output) (ValueType output))
isPKOutputFrom pkh out = case outputAddress out of
  Pl.Address (Pl.PubKeyCredential pkh') _mStCred ->
    if pkh == pkh'
      then
        Just $
          ConcreteOutput
            pkh
            (out ^. outputStakingCredentialL)
            (out ^. outputValueL)
            (out ^. outputDatumL)
      else Nothing
  _ -> Nothing

-- | Test if the value on an output contains only Ada, and adapt the return type
-- accordingly if it is so.
isOnlyAdaOutput ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (DatumType output) Pl.Ada)
isOnlyAdaOutput out =
  if Pl.isAdaOnlyValue (outputValue out)
    then
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (Pl.fromValue $ outputValue out)
          (out ^. outputDatumL)
    else Nothing

-- TODO et cetera

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
-- IMPORTANT INTERNAL: If you add or remove fields from 'TxOpts', make sure
-- to update the internal @fields@ value from 'Cooked.Tx.Constraints.Pretty'
--
-- TODO Refactor field names to avoid clashes on common terms such as "collateral" or "balance"
data TxOpts = TxOpts
  { -- | Performs an adjustment to unbalanced txs, making sure every UTxO that is produced
    --  has the necessary minimum amount of Ada.
    --
    -- By default, this is set to @False@, given this is the default behavior in Plutus:
    -- https://github.com/input-output-hk/plutus-apps/issues/143#issuecomment-1013012744
    adjustUnbalTx :: Bool,
    -- | When submitting a transaction for real (i.e., running in the 'Plutus.Contract.Contract' monad),
    --  it is common to call 'Plutus.Contract.Request.awaitTxConfirmed' after 'Plutus.Contract.Request.submitTxConstraints'.
    --  If you /do NOT/ wish to do so, please set this to @False@.
    --
    --  /This has NO effect when running outside of 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    awaitTxConfirmed :: Bool,
    -- | Whether to increase the slot counter automatically on this submission.
    -- This is useful for modelling transactions that could be submitted in parallel in reality, so there
    -- should be no explicit ordering of what comes first. One good example is in the Crowdfunding use case contract.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    autoSlotIncrease :: Bool,
    -- | Reorders the transaction outputs to fit the ordering of output
    -- constraints. Those outputs are put at the very beginning of the list.
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    forceOutputOrdering :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted ('adjustUnbalTx') and balanced. This is prefixed
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
    unsafeModTx :: [RawModTx],
    -- | Whether to balance the transaction or not. Balancing
    --  ensures that @input + mint = output + fees + burns@, if you decide to
    --  set @balance = false@ you will have trouble satisfying that equation by
    --  hand because @fees@ are variable. You will likely see a
    --  @ValueNotPreserved@ error and should adjust the fees accordingly.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @True@.
    balance :: Bool,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @AdjustExistingOutput@.
    balanceOutputPolicy :: BalanceOutputPolicy,
    -- | Which wallet to use to provide outputs for balancing and collaterals.
    -- Either the first signer by default, or an explicit wallet. In the second
    -- case, this wallet must be a signer of the transaction. This option WILL
    -- NOT ensure that it is added in case it is not already present in the
    -- list of signers.
    balanceWallet :: BalancingWallet
  }
  deriving (Eq, Show)

instance Default TxOpts where
  def =
    TxOpts
      { adjustUnbalTx = False,
        awaitTxConfirmed = True,
        autoSlotIncrease = True,
        forceOutputOrdering = True,
        unsafeModTx = [],
        balance = True,
        balanceOutputPolicy = def,
        balanceWallet = def
      }

-- * Description of the Minting

type MintsConstrs redeemer =
  ( Pl.ToData redeemer,
    Show redeemer,
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
    ( IsOutput o,
      Show (OwnerType o),
      IsTxSkelOutAllowedOwner (OwnerType o),
      Typeable (OwnerType o),
      ToCredential (OwnerType o),
      DatumType o ~ TxSkelOutDatum, -- see the [note on TxSkelOutData]
      ValueType o ~ Pl.Value -- needed for the 'txSkelOutValueL'
    ) =>
    {producedOutput :: o} ->
    TxSkelOut

deriving instance Show TxSkelOut

-- | Since we mostly care about whether the transaction outputs are the same
-- on-chain, this is sufficient:
instance Eq TxSkelOut where
  (==) = (==) `on` txSkelOutToTxOut

txSkelOutValueL :: Lens' TxSkelOut Pl.Value
txSkelOutValueL =
  lens
    (\(Pays output) -> outputValue output)
    (\(Pays output) newValue -> Pays $ output & outputValueL .~ newValue)

txSkelOutValue :: TxSkelOut -> Pl.Value
txSkelOutValue = (^. txSkelOutValueL)

txSkelOutValidator :: TxSkelOut -> Maybe (Pl.Versioned Pl.Validator)
txSkelOutValidator (Pays output) = rightToMaybe (toPKHOrValidator $ output ^. outputOwnerL)

type TxSkelOutDatumConstrs a = (Show a, Pretty a, Pl.ToData a, Pl.Eq a, Typeable a)

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

-- | The 'Pretty' instance for 'TxSkelOutDatum' relays the pretty-printing of
-- the datum it contains.
instance Pretty TxSkelOutDatum where
  pretty TxSkelOutNoDatum = mempty
  pretty (TxSkelOutDatumHash datum) = PP.pretty datum
  pretty (TxSkelOutDatum datum) = PP.pretty datum
  pretty (TxSkelOutInlineDatum datum) = PP.pretty datum

{- [note on TxSkelOut data]

On transaction outputs, we have the option to use

1. no datum
2. only a datum hash
3. a "normal" datum
4. an inline datum

These four options are also what the type 'TxSkelOutDatum' records. The
following table explains their differences.

|                | in the simulated chain state | on the 'txInfoData' | 'Pl.OutputDatum' constructor seen by the validator |
|----------------+------------------------------+---------------------+----------------------------------------------------|
| no datum       | no                           | no                  | 'Pl.NoOutputDatum'                                 |
|----------------+------------------------------+---------------------+----------------------------------------------------|
| datum hash     | yes                          | no                  | 'Pl.OutputDatumHash'                               |
|----------------+------------------------------+---------------------+----------------------------------------------------|
| "normal" datum | yes                          | yes                 | 'Pl.OutputDatumHash'                               |
|----------------+------------------------------+---------------------+----------------------------------------------------|
| inline datum   | yes                          | no                  | 'Pl.OutputDatum'                                   |
|----------------+------------------------------+---------------------+----------------------------------------------------|

That is:

- Whenever there is a datum, we'll store it in the state of our simulated
  chain. This will make it possible to retrieve it later, using functions such
  as 'datumFromHash'.

- Both of the 'TxSkelOutDatumHash' and 'TxSkelOutDatum' constructors will create
  an output that scripts see on the 'txInfo' as having a datum hash. The
  difference is whether that hash will be resolvable using validator functions
  like 'findDatum'.

In summary: On the one hand, there is the function 'txSkelOutDatumComplete'
which extracts the whole datum, with its pretty-printed representation, from a
'TxSkelOut', in order to save it in the simulated chain state. On the other
hand, there is 'txSkelOutToTxOut', which will return the output as seen on the
'txInfo' by a validator, with the correct 'Pl.OutputDatum' on it.
-}

-- | The transaction output, as seen by a validator. In particular, see the
-- [note on TxSkelOut data].
txSkelOutToTxOut :: TxSkelOut -> Pl.TxOut
txSkelOutToTxOut (Pays output) = outputTxOut output

-- | See the [note on TxSkelOut data]
txSkelOutDatumComplete :: TxSkelOut -> Maybe (Pl.Datum, Doc ())
txSkelOutDatumComplete (Pays output) = txSkelOutUntypedDatum $ output ^. outputDatumL

-- | See the [note on TxSkelOut data]
instance ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Pl.NoOutputDatum
  toOutputDatum (TxSkelOutDatumHash datum) = Pl.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
  toOutputDatum (TxSkelOutDatum datum) = Pl.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
  toOutputDatum (TxSkelOutInlineDatum datum) = Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData $ datum

txSkelOutUntypedDatum :: TxSkelOutDatum -> Maybe (Pl.Datum, Doc ())
txSkelOutUntypedDatum = \case
  TxSkelOutNoDatum -> Nothing
  TxSkelOutDatumHash x -> Just (Pl.Datum $ Pl.toBuiltinData x, PP.pretty x)
  TxSkelOutDatum x -> Just (Pl.Datum $ Pl.toBuiltinData x, PP.pretty x)
  TxSkelOutInlineDatum x -> Just (Pl.Datum $ Pl.toBuiltinData x, PP.pretty x)

txSkelOutTypedDatum :: Pl.FromData a => TxSkelOutDatum -> Maybe a
txSkelOutTypedDatum = Pl.fromBuiltinData . Pl.getDatum . fst <=< txSkelOutUntypedDatum

-- ** Smart constructors for transaction outputs

-- | Pay a certain value to a public key, without using a datum.
paysPK :: Pl.PubKeyHash -> Pl.Value -> TxSkelOut
paysPK pkh value = Pays (ConcreteOutput pkh Nothing value TxSkelOutNoDatum)

-- | Pays a script a certain value with a certain datum, which will be included
-- as a datum hash on the transaction.
paysScript ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    Pretty (Pl.DatumType a),
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
    )

-- | Like 'paysScript', but using an inline datum.
paysScriptInlineDatum ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    Pretty (Pl.DatumType a),
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
    )

-- | Like 'paysScript', but won't include the complete datum on the
-- transaction. This is only useful if there's no script that checks the output
-- datum.
paysScriptDatumHash ::
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    Pretty (Pl.DatumType a),
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
    )

-- * Redeemers for transaction inputs

type SpendsScriptConstrs a =
  ( Pl.ToData (Pl.RedeemerType a),
    Show (Pl.RedeemerType a),
    Pl.Eq (Pl.RedeemerType a),
    Typeable (Pl.RedeemerType a)
  )

data TxSkelRedeemer where
  TxSkelNoRedeemerForPK :: TxSkelRedeemer
  TxSkelNoRedeemerForScript :: TxSkelRedeemer
  TxSkelRedeemerForScript :: SpendsScriptConstrs a => Pl.RedeemerType a -> TxSkelRedeemer

txSkelTypedRedeemer :: Pl.FromData (Pl.RedeemerType a) => TxSkelRedeemer -> Maybe (Pl.RedeemerType a)
txSkelTypedRedeemer (TxSkelRedeemerForScript redeemer) = Pl.fromData . Pl.toData $ redeemer
txSkelTypedRedeemer _ = Nothing

deriving instance (Show TxSkelRedeemer)

instance Eq TxSkelRedeemer where
  TxSkelNoRedeemerForPK == TxSkelNoRedeemerForPK = True
  TxSkelNoRedeemerForScript == TxSkelNoRedeemerForScript = True
  (TxSkelRedeemerForScript r1) == (TxSkelRedeemerForScript r2) =
    case typeOf r1 `eqTypeRep` typeOf r2 of
      Just HRefl -> r1 Pl.== r2
      Nothing -> False
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
      txSkelOuts :: [TxSkelOut],
      txSkelFee :: Integer -- Fee in Lovelace
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
    ("txSkelInsCollateral", "txSkelInsCollateralL"),
    ("txSkelOuts", "txSkelOutsL"),
    ("txSkelFee", "txSkelFeeL")
  ]
  ''TxSkel

-- | A convenience template where wallet 1 is the default signer of an
-- otherwise empty transaction skeleton.
txSkelTemplate :: TxSkel
txSkelTemplate = txSkelSubmittedBy (wallet 1)

txSkelSubmittedBy :: Wallet -> TxSkel
txSkelSubmittedBy w =
  TxSkel
    { txSkelLabel = Set.empty,
      txSkelOpts = def,
      txSkelMints = Map.empty,
      txSkelValidityRange = Pl.always,
      txSkelSigners = w NEList.:| [],
      txSkelIns = Map.empty,
      txSkelOuts = [],
      txSkelFee = 0
    }

-- | Return all data on transaction outputs.
txSkelOutputData :: TxSkel -> Map Pl.DatumHash (Pl.Datum, Doc ())
txSkelOutputData =
  foldMapOf
    ( txSkelOutsL
        % folded
        % afolding txSkelOutDatumComplete -- if you're wondering why to use this function, see the [note on TxSkelOut data]
    )
    (\(datum, datumStr) -> Map.singleton (Pl.datumHash datum) (datum, datumStr))

-- | The value in all transaction inputs, plus the negative parts of the minted
-- value. This is the right hand side of the "balancing equation":
--
-- > mints + inputs = fees + burns + outputs
txSkelOutputValue :: TxSkel -> Pl.Value
txSkelOutputValue skel@TxSkel {txSkelMints = mints} =
  negativePart (txSkelMintsValue mints)
    <> foldOf (txSkelOutsL % folded % txSkelOutValueL) skel
    <> Pl.lovelaceValueOf (txSkelFee skel)

txSkelOutValidators :: TxSkel -> Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
txSkelOutValidators =
  Map.fromList
    . mapMaybe
      ( \txSkelOut ->
          let validator = txSkelOutValidator txSkelOut
           in case validator of
                Nothing -> Nothing
                Just script -> Just (Ledger.Scripts.validatorHash script, script)
      )
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
negativePart = over flattenValueI (mapMaybe (\(ac, n) -> if n < 0 then Just (ac, - n) else Nothing))
