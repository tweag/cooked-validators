-- | This module exposes outputs as they can be defined in a
-- 'Cooked.Skeleton.TxSkel' with various utilities around them.
module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutValue,
    txSkelOutValidator,
    txSkelOutOwnerTypeP,
    txSkelOutputDatumTypeAT,
    IsTxSkelOutAllowedOwner (..),
    txSkelOutReferenceScript,
    OwnerConstraints,
    ReferenceScriptConstraints,
  )
where

import Cooked.Output
import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.Value
import Cooked.Wallet
import Data.Data (cast)
import Data.Either.Combinators
import Data.Function
import Data.Maybe (fromMaybe)
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script (TypedValidator (..))
import Plutus.Script.Utils.V3.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Type.Reflection

-- | Depicts the entities that are allowed to own a 'TxSkelOut'
class IsTxSkelOutAllowedOwner a where
  toPKHOrValidator :: a -> Either Api.PubKeyHash (Script.Versioned Script.Validator)

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrValidator = Left

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrValidator = Left . walletPKHash

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrValidator = Right

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrValidator = toPKHOrValidator . Script.toVersioned @Script.Validator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Validator)) where
  toPKHOrValidator = id

instance IsTxSkelOutAllowedOwner (Script.MultiPurposeScript a) where
  toPKHOrValidator = toPKHOrValidator . Script.toVersioned @Script.Validator

-- | Type constraints over the owner of a 'TxSkelOut'
type OwnerConstraints owner =
  ( IsTxSkelOutAllowedOwner owner,
    Script.ToCredential owner,
    Typeable owner,
    Show owner
  )

-- | Type constraints over the reference script in a 'TxSkelOut'
type ReferenceScriptConstraints refScript =
  ( Script.ToVersioned Script.Script refScript,
    Show refScript,
    Typeable refScript
  )

-- | Transaction outputs. The 'Pays' constructor is really general, and you'll
-- probably want to use the 'receives' smart constructor in most cases.
data TxSkelOut where
  Pays ::
    ( Show o, -- This is needed only for the 'Show' instance of 'TxSkel', which
    -- in turn is only needed in tests.
      Typeable o,
      IsTxInfoOutput o,
      OwnerConstraints (OwnerType o),
      DatumType o ~ TxSkelOutDatum,
      ValueType o ~ TxSkelOutValue,
      ReferenceScriptConstraints (ReferenceScriptType o)
    ) =>
    o ->
    TxSkelOut

instance Eq TxSkelOut where
  Pays a == Pays b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl -> outputTxOut a == outputTxOut b
    Nothing -> False

deriving instance Show TxSkelOut

-- | Smart constructor to build a 'TxSkelOut' from an owner and payment. This
-- should be the main way of building outputs.
receives :: (Show owner, Typeable owner, IsTxSkelOutAllowedOwner owner, Script.ToCredential owner) => owner -> Payable els -> TxSkelOut
receives owner =
  go $
    Pays $
      ConcreteOutput
        owner
        Nothing -- No staking credential by default
        defaultTxSkelDatum -- Default datum defined below
        (TxSkelOutValue mempty True) -- Empty value by default, adjustable to min ada
        (Nothing @(Script.Versioned Script.Script)) -- No reference script by default
  where
    go :: TxSkelOut -> Payable els -> TxSkelOut
    go (Pays output) (VisibleHashedDatum dat) = Pays $ setDatum output $ TxSkelOutSomeDatum (DatumContent dat) HashedVisibleInTx
    go (Pays output) (InlineDatum dat) = Pays $ setDatum output $ TxSkelOutSomeDatum (DatumContent dat) Inline
    go (Pays output) (HiddenHashedDatum dat) = Pays $ setDatum output $ TxSkelOutSomeDatum (DatumContent dat) HashedHiddenInTx
    go (Pays output) (FixedValue v) = Pays $ setValue output $ TxSkelOutValue (Script.toValue v) False
    go (Pays output) (Value v) = Pays $ setValue output $ TxSkelOutValue (Script.toValue v) True
    go (Pays output) (ReferenceScript script) = Pays $ setReferenceScript output $ Script.toVersioned @Script.Script script
    go (Pays output) (StakingCredential (Script.toMaybeStakingCredential -> Just stCred)) = Pays $ setStakingCredential output stCred
    go pays (StakingCredential _) = pays
    go pays (PayableAnd p1 p2) = go (go pays p1) p2

    defaultTxSkelDatum = case toPKHOrValidator owner of
      -- V1 and V2 script always need a datum, even if empty
      Right (Script.Versioned _ v) | v <= Script.PlutusV2 -> TxSkelOutSomeDatum (DatumContent ()) HashedHiddenInTx
      -- V3 script and PKH do not necessarily need a datum
      _ -> TxSkelOutNoDatum

-- | A lens to get or set a 'TxSkelOutDatum' in a 'TxSkelOut'
txSkelOutDatumL :: Lens' TxSkelOut TxSkelOutDatum
txSkelOutDatumL =
  lens
    (\(Pays output) -> output ^. outputDatumL)
    (\(Pays output) newDatum -> Pays $ output & outputDatumL .~ newDatum)

-- | A lens to get or set a 'TxSkelOutValue' in a 'TxSkelOut'
txSkelOutValueL :: Lens' TxSkelOut TxSkelOutValue
txSkelOutValueL =
  lens
    (\(Pays output) -> output ^. outputValueL)
    (\(Pays output) newValue -> Pays $ output & outputValueL .~ newValue)

-- | Returns the value contained in a 'TxSkelOut'
txSkelOutValue :: TxSkelOut -> Api.Value
txSkelOutValue = (^. (txSkelOutValueL % txSkelOutValueContentL))

-- | Returns the optional validator owning a given 'TxSkelOut'
txSkelOutValidator :: TxSkelOut -> Maybe (Script.Versioned Script.Validator)
txSkelOutValidator (Pays output) = rightToMaybe (toPKHOrValidator $ output ^. outputOwnerL)

-- | Returns the optional reference script in a 'TxSkelOut'
txSkelOutReferenceScript :: TxSkelOut -> Maybe (Script.Versioned Script.Script)
txSkelOutReferenceScript (Pays output) = Script.toVersioned <$> (output ^. outputReferenceScriptL)

-- | Decides if a transaction output has a certain owner type.
txSkelOutOwnerTypeP ::
  forall ownerType.
  (OwnerConstraints ownerType) =>
  Prism' TxSkelOut (ConcreteOutput ownerType TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script))
txSkelOutOwnerTypeP =
  prism'
    Pays
    ( \(Pays output) ->
        case typeOf (output ^. outputOwnerL) `eqTypeRep` typeRep @ownerType of
          Just HRefl ->
            let cOut = fromAbstractOutput output
             in Just $ cOut {concreteOutputReferenceScript = Script.toVersioned <$> concreteOutputReferenceScript cOut}
          Nothing -> Nothing
    )

-- | A traversal over datums of type @a@ in a 'TxSkelOut'
txSkelOutputDatumTypeAT ::
  (Api.FromData a, Typeable a) =>
  AffineTraversal' TxSkelOut a
txSkelOutputDatumTypeAT =
  atraversal
    ( \txSkelOut@(Pays output) -> fromMaybe (Left txSkelOut) $ do
        (Api.Datum datum) <- txSkelOutUntypedDatum $ output ^. outputDatumL
        Right <$> Api.fromBuiltinData datum
    )
    ( \(Pays output) newTyDatum ->
        Pays $
          over
            outputDatumL
            ( \case
                TxSkelOutNoDatum -> TxSkelOutNoDatum
                TxSkelOutSomeDatum (DatumContent tyDatum) placement ->
                  TxSkelOutSomeDatum (DatumContent $ fromMaybe tyDatum $ cast newTyDatum) placement
            )
            output
    )
