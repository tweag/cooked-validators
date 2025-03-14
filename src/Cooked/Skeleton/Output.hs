module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutValue,
    txSkelOutValidator,
    txSkelOutOwnerTypeP,
    txSkelOutputDatumTypeAT,
  )
where

import Cooked.Conversion
import Cooked.Output
import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.Value
import Cooked.Wallet
import Data.Either.Combinators
import Data.Function
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script (TypedValidator (..))
import PlutusLedgerApi.V3 qualified as Api
import Type.Reflection

class IsTxSkelOutAllowedOwner a where
  toPKHOrValidator :: a -> Either Api.PubKeyHash (Script.Versioned Script.Validator)

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrValidator = Left

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrValidator = Left . toPubKeyHash

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrValidator = Right

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrValidator = Right . Script.tvValidator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Validator)) where
  toPKHOrValidator = id

-- | Transaction outputs. The 'Pays' constructor is really general, and you'll
-- probably want to use the 'receives' smart constructor in most cases.
data TxSkelOut where
  Pays ::
    ( Show o, -- This is needed only for the 'Show' instance of 'TxSkel', which
    -- in turn is only needed in tests.
      Typeable o,
      IsTxInfoOutput o,
      IsTxSkelOutAllowedOwner (OwnerType o),
      ToCredential (OwnerType o),
      Typeable (OwnerType o),
      DatumType o ~ TxSkelOutDatum,
      ValueType o ~ TxSkelOutValue,
      ToVersionedScript (ReferenceScriptType o),
      Show (OwnerType o),
      Show (ReferenceScriptType o),
      Typeable (ReferenceScriptType o)
    ) =>
    o ->
    TxSkelOut

instance Eq TxSkelOut where
  Pays a == Pays b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl -> outputTxOut a == outputTxOut b
    Nothing -> False

deriving instance Show TxSkelOut

-- | Smart constructor to build @TxSkelOut@ from an owner and payment. This
-- should be the main way of building outputs.
receives :: (Show owner, Typeable owner, IsTxSkelOutAllowedOwner owner, ToCredential owner) => owner -> Payable els -> TxSkelOut
receives owner =
  go $
    Pays $
      ConcreteOutput
        owner
        Nothing -- No staking credential by default
        TxSkelOutNoDatum -- No datum by default
        (TxSkelOutValue mempty True) -- Empty value by default, adjustable to min ada
        (Nothing @(Script.Versioned Script.Script)) -- No reference script by default
  where
    go :: TxSkelOut -> Payable els -> TxSkelOut
    go (Pays output) (VisibleHashedDatum dat) = Pays $ setDatum output $ TxSkelOutDatum dat
    go (Pays output) (InlineDatum dat) = Pays $ setDatum output $ TxSkelOutInlineDatum dat
    go (Pays output) (HiddenHashedDatum dat) = Pays $ setDatum output $ TxSkelOutDatumHash dat
    go (Pays output) (Value v) = Pays $ setValue output $ TxSkelOutValue (toValue v) False
    go (Pays output) (AdjustableValue v) = Pays $ setValue output $ TxSkelOutValue (toValue v) True
    go (Pays output) (ReferenceScript script) = Pays $ setReferenceScript output $ toVersionedScript script
    go (Pays output) (StakingCredential (toMaybeStakingCredential -> Just stCred)) = Pays $ setStakingCredential output stCred
    go pays (StakingCredential _) = pays
    go pays (PayableAnd p1 p2) = go (go pays p1) p2

txSkelOutDatumL :: Lens' TxSkelOut TxSkelOutDatum
txSkelOutDatumL =
  lens
    (\(Pays output) -> output ^. outputDatumL)
    (\(Pays output) newDatum -> Pays $ output & outputDatumL .~ newDatum)

txSkelOutValueL :: Lens' TxSkelOut TxSkelOutValue
txSkelOutValueL =
  lens
    (\(Pays output) -> output ^. outputValueL)
    (\(Pays output) newValue -> Pays $ output & outputValueL .~ newValue)

txSkelOutValue :: TxSkelOut -> Api.Value
txSkelOutValue = (^. (txSkelOutValueL % txSkelOutValueContentL))

txSkelOutValidator :: TxSkelOut -> Maybe (Script.Versioned Script.Validator)
txSkelOutValidator (Pays output) = rightToMaybe (toPKHOrValidator $ output ^. outputOwnerL)

-- | Decide if a transaction output has a certain owner and datum type.
txSkelOutOwnerTypeP ::
  forall ownerType.
  ( ToCredential ownerType,
    Show ownerType,
    IsTxSkelOutAllowedOwner ownerType,
    Typeable ownerType
  ) =>
  Prism' TxSkelOut (ConcreteOutput ownerType TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script))
txSkelOutOwnerTypeP =
  prism'
    Pays
    ( \(Pays output) ->
        case typeOf (output ^. outputOwnerL) `eqTypeRep` typeRep @ownerType of
          Just HRefl ->
            let cOut = fromAbstractOutput output
             in Just $ cOut {concreteOutputReferenceScript = toVersionedScript <$> concreteOutputReferenceScript cOut}
          Nothing -> Nothing
    )

txSkelOutputDatumTypeAT ::
  (Api.FromData a, Typeable a) =>
  AffineTraversal' TxSkelOut a
txSkelOutputDatumTypeAT =
  atraversal
    ( \txSkelOut -> case txSkelOutDatumComplete txSkelOut of
        Nothing -> Left txSkelOut
        Just (Api.Datum datum) -> case Api.fromBuiltinData datum of
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

    txSkelOutDatumComplete :: TxSkelOut -> Maybe Api.Datum
    txSkelOutDatumComplete (Pays output) = txSkelOutUntypedDatum $ output ^. outputDatumL
