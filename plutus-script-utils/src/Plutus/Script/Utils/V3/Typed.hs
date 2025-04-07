{-# LANGUAGE UndecidableInstances #-}

module Plutus.Script.Utils.V3.Typed
  ( ConnectionError (..),
    WrongOutTypeError (..),
    checkMultiPurposeScriptAddress,
    checkDatum,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    makeTypedScriptTxOut,
    typeScriptTxOut,
    typeScriptTxOutRef,
    MultiPurposeScriptTypes (..),
    MultiPurposeScript (..),
    generalise,
    Any,
    toCardanoAddressAny,
    IsDataDatum,
    TypedMultiPurposeScript (..),
    mkMultiPurposeScript,
    CertifyingPurposeType',
    MintingPurposeType',
    ProposingPurposeType',
    RewardingPurposeType',
    SpendingPurposeType',
    VotingPurposeType',
    CertifyingPurposeType,
    MintingPurposeType,
    ProposingPurposeType,
    RewardingPurposeType,
    SpendingPurposeType,
    VotingPurposeType,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Codec.Serialise (Serialise)
import Control.Monad (unless)
import Control.Monad.Except
  ( MonadError,
    throwError,
  )
import Data.Coerce (coerce)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address
  ( ToAddress (toAddress),
    ToCardanoAddress (toCardanoAddress),
    ToCredential (toCredential),
  )
import Plutus.Script.Utils.Data (datumHash)
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV3),
    MintingPolicy (MintingPolicy),
    Script,
    StakeValidator (StakeValidator),
    ToMintingPolicy (toMintingPolicy),
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    ToStakeValidator (toStakeValidator),
    ToStakeValidatorHash (toStakeValidatorHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator (Validator),
    ValidatorHash (ValidatorHash),
    Versioned (Versioned),
  )
import Plutus.Script.Utils.V1.Typed
  ( Any,
    ConnectionError
      ( NoDatum,
        UnknownRef,
        WrongCredentialType,
        WrongDatumType,
        WrongRedeemerType,
        WrongValidatorHash,
        WrongValidatorType
      ),
    WrongOutTypeError
      ( ExpectedPubkeyGotScript,
        ExpectedScriptGotPubkey
      ),
  )
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
  ( Address (Address, addressCredential),
    BuiltinData,
    Credential (PubKeyCredential, ScriptCredential),
    CurrencySymbol,
    Datum (Datum),
    FromData,
    OutputDatum (OutputDatum, OutputDatumHash),
    ProposalProcedure,
    ScriptHash (ScriptHash),
    ScriptInfo (CertifyingScript, MintingScript, ProposingScript, RewardingScript, SpendingScript, VotingScript),
    ToData (toBuiltinData),
    TxCert,
    TxId (TxId),
    TxInfo,
    TxOut (TxOut),
    TxOutRef (TxOutRef),
    Value,
    Voter,
  )
import PlutusTx
  ( FromData (fromBuiltinData),
    unstableMakeIsData,
  )
import PlutusTx.Prelude (BuiltinString, BuiltinUnit, check, trace, traceError)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter qualified as PP
import Prettyprinter.Extras qualified as PP

-- * Type family representing all the necessary types for a 'MultiPurposeScript'

{-- Note on @MultipPurposeScriptTypes a@. While this type class is useful to
  gather all the possible type information of a multi-purpose script, and to
  benefit from their default instances, the Plutus compiler is unable to process
  type families directly. In particular, @TypedMultiPurposeScript@ cannot be
  indexed by a certain @a@ and is instead indexed by all the type components
  directly, which is sadly inconvenient. For conveniency reasons, we do provide
  @IndexedTypedMultiPurposeScript@ as a constrained instance of
  @TypedMultiPurposeScript@ which can be used at an abstract level of script
  definition. Note of our helpers and tools can take these directly as inputs
  though, or the Plutus compiler will complain about irreducible type
  families. --}

class MultiPurposeScriptTypes a where
  -- Certifying purpose type variables with default
  type CertifyingRedeemerType a
  type CertifyingTxInfoType a

  type CertifyingRedeemerType a = ()
  type CertifyingTxInfoType a = TxInfo

  -- Minting purpose type variables with default
  type MintingRedeemerType a
  type MintingTxInfoType a

  type MintingRedeemerType a = ()
  type MintingTxInfoType a = TxInfo

  -- Proposing purpose type variables with default
  type ProposingRedeemerType a
  type ProposingTxInfoType a

  type ProposingRedeemerType a = ()
  type ProposingTxInfoType a = TxInfo

  -- Rewarding purpose type variables with default
  type RewardingRedeemerType a
  type RewardingTxInfoType a

  type RewardingRedeemerType a = ()
  type RewardingTxInfoType a = TxInfo

  -- Spending purpose type variables with default
  type SpendingRedeemerType a
  type SpendingTxInfoType a
  type SpendingDatumType a

  type SpendingRedeemerType a = ()
  type SpendingTxInfoType a = TxInfo
  type SpendingDatumType a = ()

  -- Voting purpose type variables with default
  type VotingRedeemerType a
  type VotingTxInfoType a

  type VotingRedeemerType a = ()
  type VotingTxInfoType a = TxInfo

instance MultiPurposeScriptTypes ()

instance MultiPurposeScriptTypes Void where
  type CertifyingRedeemerType Void = Void
  type MintingRedeemerType Void = Void
  type ProposingRedeemerType Void = Void
  type RewardingRedeemerType Void = Void
  type SpendingRedeemerType Void = Void
  type SpendingDatumType Void = Void
  type VotingRedeemerType Void = Void

instance MultiPurposeScriptTypes Any where
  type CertifyingRedeemerType Any = BuiltinData
  type MintingRedeemerType Any = BuiltinData
  type ProposingRedeemerType Any = BuiltinData
  type RewardingRedeemerType Any = BuiltinData
  type SpendingRedeemerType Any = BuiltinData
  type SpendingDatumType Any = BuiltinData
  type VotingRedeemerType Any = BuiltinData

-- * Various script purposes

{-- Note on types for the script purposes: Each script purpose is associated with
  a given function type with the following characteristics:

  - These purposes return a boolean which corresponds to the validation result

  - These purposes all take a redeemer of a dedicated type, which varies between
    the various purposes. Having a custom type here is mandatory because the
    concrete redeemer type is only known by the user. It also allows to
    customize the way deserialization of the redeemer is performed.

  - The purposes all take a view on the @TxInfo@ from Plutus as parameter. This
    type is also custom (although it is expected that it will be TxInfo itself
    in some cases) to allows users to perform partial deserialization of the
    TxInfo. It happens very seldom that all the fields of the TxInfo are
    actually useful for a given script and this is advises to use a custom type
    there instead of TxInfo itself.

  - The purposes all take additional parameters based on what can be retrieved
    from the @scriptContextScriptInfo@ of the script context (which has to be
    deserialized). For instance, a minting purpose will receive its own currency
    symbol as a parameter.

  - The spending purpose takes an optional datum among these additional
    parameters which, similarly to the transaction info and redeemer, is given a
    generic type.
--}

-- | Certifying scripts take an index, certificate, redeemer and txInfo
type CertifyingPurposeType' red txInfo = Integer -> TxCert -> red -> txInfo -> Bool

type CertifyingPurposeType a = CertifyingPurposeType' (CertifyingRedeemerType a) (CertifyingTxInfoType a)

-- | Minting scripts take their own currency symbol, redeemer and txInfo
type MintingPurposeType' red txInfo = CurrencySymbol -> red -> txInfo -> Bool

type MintingPurposeType a = MintingPurposeType' (MintingRedeemerType a) (MintingTxInfoType a)

-- | Proposing scripts take an index, proposal, redeemer and txInfo
type ProposingPurposeType' red txInfo = Integer -> ProposalProcedure -> red -> txInfo -> Bool

type ProposingPurposeType a = ProposingPurposeType' (ProposingRedeemerType a) (ProposingTxInfoType a)

-- | Rewarding scripts take a credential, redeemer and txInfo
type RewardingPurposeType' red txInfo = Credential -> red -> txInfo -> Bool

type RewardingPurposeType a = RewardingPurposeType' (RewardingRedeemerType a) (RewardingTxInfoType a)

-- | Spending scripts take the utxo being consumed, an optional datum, redeemer and txInfo
type SpendingPurposeType' dat red txInfo = TxOutRef -> Maybe dat -> red -> txInfo -> Bool

type SpendingPurposeType a = SpendingPurposeType' (SpendingDatumType a) (SpendingRedeemerType a) (SpendingTxInfoType a)

-- | Voting scripts take a voter, redeemer and txInfo
type VotingPurposeType' red txInfo = Voter -> red -> txInfo -> Bool

type VotingPurposeType a = VotingPurposeType' (VotingRedeemerType a) (VotingTxInfoType a)

-- * Typed multi-purpose scripts

-- | Typed multi-purpose scripts provide 6 different purposes, each with their
-- own types as described above.
data
  TypedMultiPurposeScript
    certifyingRed
    certifyingTxInfo
    mintingRed
    mintingTxInfo
    proposingRed
    proposingTxInfo
    rewardingRed
    rewardingTxInfo
    spendingDat
    spendingRed
    spendingTxInfo
    votingRed
    votingTxInfo = ( FromData certifyingRed,
                     FromData certifyingTxInfo,
                     FromData mintingRed,
                     FromData mintingTxInfo,
                     FromData proposingRed,
                     FromData proposingTxInfo,
                     FromData rewardingRed,
                     FromData rewardingTxInfo,
                     FromData spendingDat,
                     FromData spendingRed,
                     FromData spendingTxInfo,
                     FromData votingRed,
                     FromData votingTxInfo
                   ) =>
  TypedMultiPurposeScript
  { certifyingPurpose :: Maybe (CertifyingPurposeType' certifyingRed certifyingTxInfo),
    mintingPurpose :: Maybe (MintingPurposeType' mintingRed mintingTxInfo),
    proposingPurpose :: Maybe (ProposingPurposeType' proposingRed proposingTxInfo),
    rewardingPurpose :: Maybe (RewardingPurposeType' rewardingRed rewardingTxInfo),
    spendingPurpose :: Maybe (SpendingPurposeType' spendingDat spendingRed spendingTxInfo),
    votingPurpose :: Maybe (VotingPurposeType' votingRed votingTxInfo)
  }

-- * Compiled multi-purpose scripts

-- | A 'MultiPurposeScript' is a 'Script' with a phantom connection type
newtype MultiPurposeScript a = MultiPurposeScript {getMultiPurposeScript :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving (PP.Pretty) via (PP.PrettyShow (MultiPurposeScript a))

instance Show (MultiPurposeScript a) where
  show = ("Multi-purpose script #" <>) . show . toScriptHash

instance ToScript (MultiPurposeScript a) where
  toScript = getMultiPurposeScript

instance ToVersioned Script (MultiPurposeScript a) where
  toVersioned = (`Versioned` PlutusV3) . toScript

instance ToScriptHash (MultiPurposeScript a) where
  toScriptHash = toScriptHash . toVersioned @Script

instance ToCredential (MultiPurposeScript a) where
  toCredential = ScriptCredential . toScriptHash

instance ToAddress (MultiPurposeScript a) where
  toAddress = (`Address` Nothing) . toCredential

instance ToValidator (MultiPurposeScript a) where
  toValidator = Validator . toScript

instance ToVersioned Validator (MultiPurposeScript a) where
  toVersioned = (`Versioned` PlutusV3) . toValidator

instance ToValidatorHash (MultiPurposeScript a) where
  toValidatorHash = toValidatorHash . toScriptHash

instance ToMintingPolicy (MultiPurposeScript a) where
  toMintingPolicy = MintingPolicy . toScript

instance ToVersioned MintingPolicy (MultiPurposeScript a) where
  toVersioned = (`Versioned` PlutusV3) . MintingPolicy . toScript

instance ToMintingPolicyHash (MultiPurposeScript a) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash

instance ToStakeValidator (MultiPurposeScript a) where
  toStakeValidator = StakeValidator . toScript

instance ToVersioned StakeValidator (MultiPurposeScript a) where
  toVersioned = (`Versioned` PlutusV3) . StakeValidator . toScript

instance ToStakeValidatorHash (MultiPurposeScript a) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash

instance ToCardanoAddress (MultiPurposeScript a) where
  toCardanoAddress networkId = toCardanoAddress networkId . toVersioned @Script

toCardanoAddressAny :: C.Api.NetworkId -> MultiPurposeScript a -> C.Api.AddressAny
toCardanoAddressAny nid tv =
  case toCardanoAddress nid tv of
    C.Api.AddressInEra C.Api.ShelleyAddressInEra {} addr -> C.Api.AddressShelley addr
    C.Api.AddressInEra C.Api.ByronAddressInAnyEra {} addr -> C.Api.AddressByron addr

-- | Generalise the multi-purpose script to one that works with the 'Data' type.
-- we can do this safely because the on-chain validators are untyped, so they
-- always take 'BuiltinData' arguments. The validator script stays the same, so
-- the conversion from 'BuiltinData' to 'a' still takes place, even if it's not
-- reflected in the type signature anymore.
generalise :: MultiPurposeScript a -> MultiPurposeScript Any
generalise = coerce

-- * From 'TypedMultiPurposeScript' to 'MultiPurposeScript'

{-- Note on converting 'TypedMultiPurposeScript' to their on-chain representation:
  'TypedMultiPurposeScript' is an abstraction over actual scripts. It splits the
  various purposes into manageable entities. However, the on-chain script is a
  single function from 'BuiltinData' to 'BuiltinUnit'. In order to convert the
  former to the latter, we follow these two steps:

  - We define a custom script context where only the script info is
    deserialized, minimizing the cost and leaving to the various purpose the
    opportunity to handle the deserialization of the @TxInfo@ the way they require.

  - We define a function 'mkMultiPurposeScript' which wraps the various purposes
    into an actual script. The function deserializes the script info, case
    splits on it and delegates the work to the right script purpose. On the way,
    some tracing is performed when errors occurs, tracing that will be erased by
    the plutus compiler by default but can be kept if needed.

--}

-- | Custom script context to retrieve the script info
data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData,
    scriptContextRedeemer :: BuiltinData,
    scriptContextScriptInfo :: ScriptInfo
  }

unstableMakeIsData ''ScriptContextResolvedScriptInfo

{-# INLINEABLE mkMultiPurposeScript #-}
mkMultiPurposeScript ::
  TypedMultiPurposeScript cRed cTxInfo mRed mTxInfo pRed pTxInfo rRed rTxInfo sDat sRed sTxInfo vRed vTxInfo -> BuiltinData -> BuiltinUnit
mkMultiPurposeScript TypedMultiPurposeScript {..} dat =
  either traceError check $ do
    ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
    case scriptContextScriptInfo of
      CertifyingScript i cert | Just cPurpose <- certifyingPurpose -> do
        (red, txInfo) <- deserializeContext "certifying" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Certifying" $ cPurpose i cert red txInfo
      CertifyingScript {} -> traceError "Unsupported purpose: Certifying"
      MintingScript cur | Just mPurpose <- mintingPurpose -> do
        (red, txInfo) <- deserializeContext "minting" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Minting" $ mPurpose cur red txInfo
      MintingScript {} -> traceError "Unsupported purpose: Minting"
      ProposingScript i prop | Just pPurpose <- proposingPurpose -> do
        (red, txInfo) <- deserializeContext "proposing" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Proposing" $ pPurpose i prop red txInfo
      ProposingScript {} -> traceError "Unsupported purpose: Proposing"
      RewardingScript cred | Just rPurpose <- rewardingPurpose -> do
        (red, txInfo) <- deserializeContext "rewarding" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Rewarding" $ rPurpose cred red txInfo
      RewardingScript {} -> traceError "Unsupported purpose: Rewarding"
      SpendingScript oRef mDat | Just sPurpose <- spendingPurpose -> do
        (red, txInfo) <- deserializeContext "spending" scriptContextRedeemer scriptContextTxInfo
        mResolvedDat <- case mDat of
          Nothing -> return Nothing
          Just (Datum bDat) -> Just <$> fromBuiltinDataEither "datum" bDat
        return $ traceRunning "Spending" $ sPurpose oRef mResolvedDat red txInfo
      SpendingScript {} -> traceError "Unsupported purpose: Spending"
      VotingScript voter | Just vPurpose <- votingPurpose -> do
        (red, txInfo) <- deserializeContext "voting" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Voting" $ vPurpose voter red txInfo
      VotingScript {} -> traceError "Unsupported purpose: Voting"
  where
    fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " PlutusTx.<> name) Right . fromBuiltinData

    traceRunning :: BuiltinString -> Bool -> Bool
    traceRunning name = trace ("Running the validator with the " PlutusTx.<> name PlutusTx.<> " script purpose")

    deserializeContext :: (FromData a, FromData b) => BuiltinString -> BuiltinData -> BuiltinData -> Either BuiltinString (a, b)
    deserializeContext name redData txInfoData = do
      red <- fromBuiltinDataEither (name PlutusTx.<> " redeemer") redData
      txInfo <- fromBuiltinDataEither (name PlutusTx.<> " tx info") txInfoData
      return (red, txInfo)

-- * Checks around typed multi-purpose scripts

-- | Checks that the given validator hash is consistent with the actual validator.
checkMultiPurposeScriptAddress ::
  (MonadError ConnectionError m) => MultiPurposeScript a -> Address -> m ()
checkMultiPurposeScriptAddress (toScriptHash -> ScriptHash mpScriptHash) (Address (ScriptCredential (ScriptHash sHash)) _) =
  unless (sHash == mpScriptHash) $
    throwError $
      WrongValidatorHash (ValidatorHash mpScriptHash) (ValidatorHash sHash)
checkMultiPurposeScriptAddress _ _ = throwError $ WrongCredentialType ExpectedScriptGotPubkey

-- | Checks that the given datum has the right type.
checkDatum ::
  (FromData (SpendingDatumType a), MonadError ConnectionError m) =>
  MultiPurposeScript a ->
  Datum ->
  m (SpendingDatumType a)
checkDatum _ (Datum d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d

type IsDataDatum a = (FromData (SpendingDatumType a), ToData (SpendingDatumType a))

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (IsDataDatum a) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: SpendingDatumType a
  }

deriving instance (Eq (SpendingDatumType a)) => Eq (TypedScriptTxOut a)

-- | Create a 'TypedScriptTxOut' from a correctly-typed data script, an address, and a value.
makeTypedScriptTxOut ::
  (IsDataDatum out) =>
  MultiPurposeScript out ->
  SpendingDatumType out ->
  Value ->
  TypedScriptTxOut out
makeTypedScriptTxOut (toScriptHash -> mpScriptHash) dat val =
  TypedScriptTxOut
    ( TxOut
        (Address (ScriptCredential mpScriptHash) Nothing)
        val
        (OutputDatumHash $ datumHash $ Datum $ toBuiltinData dat)
        Nothing
    )
    dat

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut mp (TxOutRef (TxId txId) ix) txOut@(TxOut addr _ dat _) datum =
  case addressCredential addr of
    PubKeyCredential _ ->
      throwError $ WrongCredentialType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case dat of
        OutputDatum d
          | datumHash datum == datumHash d ->
              checkMultiPurposeScriptAddress mp addr
                >> TypedScriptTxOut txOut <$> checkDatum mp datum
        OutputDatumHash dh
          | datumHash datum == dh ->
              checkMultiPurposeScriptAddress mp addr
                >> TypedScriptTxOut txOut <$> checkDatum mp datum
        _ -> throwError $ NoDatum (V1.TxOutRef (V1.TxId txId) ix) (datumHash datum)

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

deriving instance (Eq (SpendingDatumType a)) => Eq (TypedScriptTxOutRef a)

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOutRef ::
  ( FromData (SpendingDatumType out),
    ToData (SpendingDatumType out),
    MonadError ConnectionError m
  ) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef mp txOutRef txOut datum =
  TypedScriptTxOutRef txOutRef <$> typeScriptTxOut mp txOutRef txOut datum
