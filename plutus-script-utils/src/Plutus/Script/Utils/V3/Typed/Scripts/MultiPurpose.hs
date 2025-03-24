{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address qualified as PSU
import Plutus.Script.Utils.Scripts qualified as PSU
import PlutusLedgerApi.V1.Address qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal
import PlutusTx.IsData
import PlutusTx.Prelude
import PlutusTx.TH
import Prettyprinter qualified as PP
import Prettyprinter.Extras qualified as PP
import Prelude qualified as HS

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
  type CertifyingTxInfoType a = Api.TxInfo

  -- Minting purpose type variables with default
  type MintingRedeemerType a
  type MintingTxInfoType a

  type MintingRedeemerType a = ()
  type MintingTxInfoType a = Api.TxInfo

  -- Proposing purpose type variables with default
  type ProposingRedeemerType a
  type ProposingTxInfoType a

  type ProposingRedeemerType a = ()
  type ProposingTxInfoType a = Api.TxInfo

  -- Rewarding purpose type variables with default
  type RewardingRedeemerType a
  type RewardingTxInfoType a

  type RewardingRedeemerType a = ()
  type RewardingTxInfoType a = Api.TxInfo

  -- Spending purpose type variables with default
  type SpendingRedeemerType a
  type SpendingTxInfoType a
  type SpendingDatumType a

  type SpendingRedeemerType a = ()
  type SpendingTxInfoType a = Api.TxInfo
  type SpendingDatumType a = ()

  -- Voting purpose type variables with default
  type VotingRedeemerType a
  type VotingTxInfoType a

  type VotingRedeemerType a = ()
  type VotingTxInfoType a = Api.TxInfo

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

  Additionally, we give default implementation for each of those cases, that
  always return either True of False. In some cases, we also give more advances
  implementation, such as forwarding minting policies.

--}

-- | Certifying scripts take an index, certificate, redeemer and txInfo
type CertifyingScriptType red txInfo = Integer -> Api.TxCert -> red -> txInfo -> Bool

-- | Default certifying scripts
trueCertifyingScript, falseCertifyingScript :: CertifyingScriptType red txInfo
trueCertifyingScript _ _ _ _ = True
falseCertifyingScript _ _ _ _ = False

-- | Minting scripts take their own currency symbol, redeemer and txInfo
type MintingScriptType red txInfo = Api.CurrencySymbol -> red -> txInfo -> Bool

-- | Default minting scripts
trueMintingScript, falseMintingScript :: MintingScriptType red txInfo
trueMintingScript _ _ _ = True
falseMintingScript _ _ _ = False

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingScript #-}
forwardingMintingScript :: (PSU.ToScriptHash script) => script -> (mtx -> [Api.TxInInfo]) -> MintingScriptType red mtx
forwardingMintingScript (PSU.toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | Api.TxInInfo _ (Api.TxOut (Api.Address (Api.ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingScript #-}
ownForwardingMintingScript :: (mtx -> [Api.TxInInfo]) -> MintingScriptType red mtx
ownForwardingMintingScript toTxInInfos cs = forwardingMintingScript cs toTxInInfos cs

-- | Proposing scripts take an index, proposal, redeemer and txInfo
type ProposingScriptType red txInfo = Integer -> Api.ProposalProcedure -> red -> txInfo -> Bool

-- | Default proposing scripts
trueProposingScript, falseProposingScript :: ProposingScriptType red txInfo
trueProposingScript _ _ _ _ = True
falseProposingScript _ _ _ _ = False

-- | Rewarding scripts take a credential, redeemer and txInfo
type RewardingScriptType red txInfo = Api.Credential -> red -> txInfo -> Bool

-- | Default rewarding scripts
trueRewardingScript, falseRewardingScript :: RewardingScriptType red txInfo
trueRewardingScript _ _ _ = True
falseRewardingScript _ _ _ = False

-- | Spending scripts take the utxo being consumed, an optional datum, redeemer and txInfo
type SpendingScriptType dat red txInfo = Api.TxOutRef -> Maybe dat -> red -> txInfo -> Bool

-- | Default spending scripts
trueSpendingScript, falseSpendingScript :: SpendingScriptType dat red txInfo
trueSpendingScript _ _ _ _ = True
falseSpendingScript _ _ _ _ = False

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingScript #-}
forwardingSpendingScript :: (PSU.ToScriptHash script) => script -> (txInfo -> Api.MintValue) -> SpendingScriptType dat red txInfo
forwardingSpendingScript (PSU.toScriptHash -> Api.ScriptHash hash) toMintValue _ _ _ (Api.mintValueToMap . toMintValue -> mintValue) =
  Api.CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingScript #-}
ownForwardingSpendingScript :: (txInfo -> Api.MintValue) -> (txInfo -> [Api.TxInInfo]) -> SpendingScriptType dat red txInfo
ownForwardingSpendingScript toMintValue toTxInInfos oRef dat red txInfo =
  case [hash | Api.TxInInfo ref (Api.TxOut (Api.Address (Api.ScriptCredential hash) _) _ _ _) <- toTxInInfos txInfo, ref == oRef] of
    [hash] -> forwardingSpendingScript hash toMintValue oRef dat red txInfo
    _ -> False

-- \| Should never happen

-- | Voting scripts take a voter, redeemer and txInfo
type VotingScriptType red txInfo = Api.Voter -> red -> txInfo -> Bool

-- | Default voting scripts
trueVotingScript, falseVotingScript :: VotingScriptType red txInfo
trueVotingScript _ _ _ = True
falseVotingScript _ _ _ = False

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
    votingTxInfo = TypedMultiPurposeScript
  { certifyingTypedScript :: CertifyingScriptType certifyingRed certifyingTxInfo,
    mintingTypedScript :: MintingScriptType mintingRed mintingTxInfo,
    proposingTypedScript :: ProposingScriptType proposingRed proposingTxInfo,
    rewardingTypedScript :: RewardingScriptType rewardingRed rewardingTxInfo,
    spendingTypedScript :: SpendingScriptType spendingDat spendingRed spendingTxInfo,
    votingTypedScript :: VotingScriptType votingRed votingTxInfo
  }

-- | Indexed typed multi-purpose scripts are variant of the above indexed with a
-- type parameter indexing all the other types. This is very convenient but
-- cannot be used as is with the Plutus compiler as mentioned in the notes about
-- @MultiPurposeScriptTypes@.
type IndexedTypedMultiPurposeScript a =
  TypedMultiPurposeScript
    (CertifyingRedeemerType a)
    (CertifyingTxInfoType a)
    (MintingRedeemerType a)
    (MintingTxInfoType a)
    (ProposingRedeemerType a)
    (ProposingTxInfoType a)
    (RewardingRedeemerType a)
    (RewardingTxInfoType a)
    (SpendingDatumType a)
    (SpendingRedeemerType a)
    (SpendingTxInfoType a)
    (VotingRedeemerType a)
    (VotingTxInfoType a)

-- * Building multi purpose scripts

{-- Note on building multi-purpose scripts. Multi-purpose scripts are never meant
  to be instantiated directly. The rationale is that most script will seldom be
  used for more than 2 purposes. Thus it is more convient to start from a
  default instance and build up the script from there. To that end, we provide
  three facilities:

  - 2 default instances, one that say No for every purpose, and one that says
    Yes for every purpose. Most likely, the former will be the right starting
    point in almost every cases.

  - Helpers @withXXXpurpose@ to override a given purpose

  - Helpers @addXXXpurpose@ to add constraints to an existing purpose. While
    this can be detrimental to use this when the parameters need to be
    deserialized in several constraints, it can prove useful when working with
    several unrelated constraints. Using this is left at the user's discretion.

  Example: if you want a script that fails for all purposes but spending and
  always succeeds otherwise, use:
  @falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingScript@

--}

falseTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () ()
falseTypedMultiPurposeScript =
  TypedMultiPurposeScript falseCertifyingScript falseMintingScript falseProposingScript falseRewardingScript falseSpendingScript falseVotingScript

trueTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () ()
trueTypedMultiPurposeScript =
  TypedMultiPurposeScript trueCertifyingScript trueMintingScript trueProposingScript trueRewardingScript trueSpendingScript trueVotingScript

-- | Overrides the certifying purpose
withCertifyingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  CertifyingScriptType cr' ctx' ->
  TypedMultiPurposeScript cr' ctx' mr mtx pr ptx rr rtx sd sr stx vr vtx
withCertifyingPurpose ts cs = ts {certifyingTypedScript = cs}

-- | Combines a new certifying purpose with the existing one
addCertifyingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  CertifyingScriptType cr ctx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addCertifyingPurpose ts cs =
  ts `withCertifyingPurpose` \ix cert red txInfo ->
    certifyingTypedScript ts ix cert red txInfo
      && cs ix cert red txInfo

-- | Overrides the certifying purpose
withMintingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  MintingScriptType mr' mtx' ->
  TypedMultiPurposeScript cr ctx mr' mtx' pr ptx rr rtx sd sr stx vr vtx
withMintingPurpose ts ms = ts {mintingTypedScript = ms}

-- | Combines a new minting purpose with the existing one
addMintingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  MintingScriptType mr mtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addMintingPurpose ts ms =
  ts `withMintingPurpose` \cs red txInfo ->
    mintingTypedScript ts cs red txInfo
      && ms cs red txInfo

-- | Overrides the proposing purpose
withProposingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  ProposingScriptType pr' ptx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr' ptx' rr rtx sd sr stx vr vtx
withProposingPurpose ts ps = ts {proposingTypedScript = ps}

-- | Combines a new proposing purpose with the existing one
addProposingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  ProposingScriptType pr ptx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addProposingPurpose ts ps =
  ts `withProposingPurpose` \ix prop red txInfo ->
    proposingTypedScript ts ix prop red txInfo
      && ps ix prop red txInfo

-- | Overrides the rewardings purpose
withRewardingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  RewardingScriptType rr' rtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr' rtx' sd sr stx vr vtx
withRewardingPurpose ts rs = ts {rewardingTypedScript = rs}

-- | Combines a new rewardings purpose with the existing one
addRewardingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  RewardingScriptType rr rtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addRewardingPurpose ts rs =
  ts `withRewardingPurpose` \cred red txInfo ->
    rewardingTypedScript ts cred red txInfo
      && rs cred red txInfo

-- | Overrides the spending purpose
withSpendingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  SpendingScriptType sd' sr' stx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd' sr' stx' vr vtx
withSpendingPurpose ts ss = ts {spendingTypedScript = ss}

-- | Combines a new spending purpose with the existing one
addSpendingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  SpendingScriptType sd sr stx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addSpendingPurpose ts ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo ->
    spendingTypedScript ts oRef mDat red txInfo
      && ss oRef mDat red txInfo

-- | Overrides the voting purpose
withVotingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  VotingScriptType vr' vtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr' vtx'
withVotingPurpose ts vs = ts {votingTypedScript = vs}

-- | Combines a new voting purpose with the existing one
addVotingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  VotingScriptType vr vtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addVotingPurpose ts vs =
  ts `withVotingPurpose` \voter red txInfo ->
    votingTypedScript ts voter red txInfo
      && vs voter red txInfo

-- * Compiled multi purpose scripts

-- | @MultiPurposeScript@ is just a wrapper around a script, such as
-- @Validator@, @MintingPolicy@ and @StakingCredential@. The only difference
-- with those is that is comes with a phantom type that can be used to reference
-- the type family from which it was built.
newtype MultiPurposeScript a = MultiPurposeScript {getMultiPurposeScript :: PSU.Script}
  deriving stock (Generic)
  deriving newtype (HS.Eq, HS.Ord, Serialise)
  deriving (PP.Pretty) via (PP.PrettyShow (MultiPurposeScript a))

instance HS.Show (MultiPurposeScript a) where
  show = ("Multi-purpose script #" <>) . HS.show . PSU.toScriptHash

instance PSU.ToScript (MultiPurposeScript a) where
  toScript = getMultiPurposeScript

instance PSU.ToVersioned PSU.Script (MultiPurposeScript a) where
  toVersioned = (`PSU.Versioned` PSU.PlutusV3) . PSU.toScript

instance PSU.ToScriptHash (MultiPurposeScript a) where
  toScriptHash = PSU.toScriptHash . PSU.toVersioned @PSU.Script

instance PSU.ToCredential (MultiPurposeScript a) where
  toCredential = Api.ScriptCredential . PSU.toScriptHash

instance PSU.ToAddress (MultiPurposeScript a) where
  toAddress = (`Api.Address` Nothing) . PSU.toCredential

instance PSU.ToValidator (MultiPurposeScript a) where
  toValidator = PSU.Validator . PSU.toScript

instance PSU.ToVersioned PSU.Validator (MultiPurposeScript a) where
  toVersioned = (`PSU.Versioned` PSU.PlutusV3) . PSU.toValidator

instance PSU.ToValidatorHash (MultiPurposeScript a) where
  toValidatorHash = PSU.toValidatorHash . PSU.toScriptHash

instance PSU.ToMintingPolicy (MultiPurposeScript a) where
  toMintingPolicy = PSU.MintingPolicy . PSU.toScript

instance PSU.ToVersioned PSU.MintingPolicy (MultiPurposeScript a) where
  toVersioned = (`PSU.Versioned` PSU.PlutusV3) . PSU.MintingPolicy . PSU.toScript

instance PSU.ToMintingPolicyHash (MultiPurposeScript a) where
  toMintingPolicyHash = PSU.toMintingPolicyHash . PSU.toScriptHash

instance PSU.ToStakeValidator (MultiPurposeScript a) where
  toStakeValidator = PSU.StakeValidator . PSU.toScript

instance PSU.ToVersioned PSU.StakeValidator (MultiPurposeScript a) where
  toVersioned = (`PSU.Versioned` PSU.PlutusV3) . PSU.StakeValidator . PSU.toScript

instance PSU.ToStakeValidatorHash (MultiPurposeScript a) where
  toStakeValidatorHash = PSU.toStakeValidatorHash . PSU.toScriptHash

-- * From @TypedMultiPurposeScript@ to @MultiPurposeScript@

{-- Note on converting @TypedMultiPurposeScript@ to their on-chain representation:
  @TypedMultiPurposeScript@ is an abstraction over actual scripts. It splits the
  various purposes into manageable entities. However, the on-chain script is a
  single function from @BuiltinData@ to @BuiltinUnit@. In order to convert the
  former to the latter, we follow these two steps:

  - We define a custom script context where only the script info is
    deserialized, minimizing the cost and leaving to the various purpose the
    opportunity to handle the deserialization of the @TxInfo@ the way they require.

  - We define a function @mkMultiPurposeScript@ which wraps the various purposes
    into an actual script. The function deserializes the script info, case
    splits on it and delegates the work to the right script purpose. On the way,
    some tracing is performed when errors occurs, tracing that will be erased by
    the plutus compiler by default but can be kept if needed.

--}

-- | Custom script context to retrieve the script info
data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData,
    scriptContextRedeemer :: BuiltinData,
    scriptContextScriptInfo :: Api.ScriptInfo
  }

unstableMakeIsData ''ScriptContextResolvedScriptInfo

{-# INLINEABLE mkMultiPurposeScript #-}
mkMultiPurposeScript ::
  ( FromData cRed,
    FromData cTxInfo,
    FromData mRed,
    FromData mTxInfo,
    FromData pRed,
    FromData pTxInfo,
    FromData rRed,
    FromData rTxInfo,
    FromData sDat,
    FromData sRed,
    FromData sTxInfo,
    FromData vRed,
    FromData vTxInfo
  ) =>
  TypedMultiPurposeScript cRed cTxInfo mRed mTxInfo pRed pTxInfo rRed rTxInfo sDat sRed sTxInfo vRed vTxInfo ->
  BuiltinData ->
  BuiltinUnit
mkMultiPurposeScript (TypedMultiPurposeScript certifyingPurpose mintingPurpose proposingPurpose rewardingPurpose spendingPurpose votingPurpose) dat =
  either traceError check $ do
    ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
    case scriptContextScriptInfo of
      Api.MintingScript cur -> do
        (red, txInfo) <- deserializeContext "minting" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Minting" $ mintingPurpose cur red txInfo
      Api.SpendingScript oRef mDat -> do
        (red, txInfo) <- deserializeContext "spending" scriptContextRedeemer scriptContextTxInfo
        mResolvedDat <- case mDat of
          Nothing -> return Nothing
          Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "datum" bDat
        return $ traceRunning "Spending" $ spendingPurpose oRef mResolvedDat red txInfo
      Api.RewardingScript cred -> do
        (red, txInfo) <- deserializeContext "rewarding" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Rewarding" $ rewardingPurpose cred red txInfo
      Api.CertifyingScript i cert -> do
        (red, txInfo) <- deserializeContext "certifying" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Certifying" $ certifyingPurpose i cert red txInfo
      Api.VotingScript voter -> do
        (red, txInfo) <- deserializeContext "voting" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Voting" $ votingPurpose voter red txInfo
      Api.ProposingScript i prop -> do
        (red, txInfo) <- deserializeContext "proposing" scriptContextRedeemer scriptContextTxInfo
        return $ traceRunning "Proposing" $ proposingPurpose i prop red txInfo
  where
    fromBuiltinDataEither :: (FromData a) => BuiltinString -> BuiltinData -> Either BuiltinString a
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . fromBuiltinData

    traceRunning :: BuiltinString -> Bool -> Bool
    traceRunning name = PlutusTx.Prelude.trace ("Running the validator with the " <> name <> " script purpose")

    deserializeContext :: (FromData a, FromData b) => BuiltinString -> BuiltinData -> BuiltinData -> Either BuiltinString (a, b)
    deserializeContext name redData txInfoData = do
      red <- fromBuiltinDataEither (name <> " redeemer") redData
      txInfo <- fromBuiltinDataEither (name <> " tx info") txInfoData
      return (red, txInfo)

-- * Creating and compiling multi-purpose scripts

{-- Note on creating and compiling multi-purpose scripts: We provide two examples
  below to showcase how one can go from their abstract script reprentation to a
  compiled plutus scripts. The steps are as follows:

  1. Start from an existing template, such as @falseTypedMultiPurposeScript@

  2. Define the typed logics for the necessary purposes

  3. Assign these logics within the template

  4. Generate the script from the typed template

  5. Compile the script and wrap it into a @MultiPurposeScript@ enveloppe

--}

trueMintingMPScript :: MultiPurposeScript a
trueMintingMPScript = MultiPurposeScript $ PSU.toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript `withMintingPurpose` trueMintingScript @() @()

trueSpendingMPScript :: MultiPurposeScript a
trueSpendingMPScript = MultiPurposeScript $ PSU.toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingScript @() @() @()

falseMPScript :: MultiPurposeScript a
falseMPScript = MultiPurposeScript $ PSU.toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript

multiPurposeScriptValue :: MultiPurposeScript a -> Api.BuiltinByteString -> Integer -> Api.Value
multiPurposeScriptValue mpScript bs = Api.singleton (PSU.scriptCurrencySymbol mpScript) (Api.TokenName bs)
