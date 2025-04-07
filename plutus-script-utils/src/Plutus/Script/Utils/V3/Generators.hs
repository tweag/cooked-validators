{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V3.Generators
  ( trueCertifyingScript,
    falseCertifyingScript,
    trueMintingScript,
    falseMintingScript,
    forwardingMintingScript,
    ownForwardingMintingScript,
    trueProposingScript,
    falseProposingScript,
    trueRewardingScript,
    falseRewardingScript,
    trueSpendingScript,
    falseSpendingScript,
    forwardingSpendingScript,
    ownForwardingSpendingScript,
    trueVotingScript,
    falseVotingScript,
    falseTypedMultiPurposeScript,
    trueTypedMultiPurposeScript,
    withCertifyingPurpose,
    addCertifyingPurpose,
    withMintingPurpose,
    addMintingPurpose,
    withProposingPurpose,
    addProposingPurpose,
    withRewardingPurpose,
    addRewardingPurpose,
    withSpendingPurpose,
    addSpendingPurpose,
    withVotingPurpose,
    addVotingPurpose,
    trueMintingMPScript,
    trueSpendingMPScript,
    falseMPScript,
    multiPurposeScriptValue,
  )
where

import Plutus.Script.Utils.Scripts
  ( ScriptHash (ScriptHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    toCurrencySymbol,
  )
import Plutus.Script.Utils.V3.Typed
  ( CertifyingScriptType,
    MintingScriptType,
    MultiPurposeScript (MultiPurposeScript),
    ProposingScriptType,
    RewardingScriptType,
    SpendingScriptType,
    TypedMultiPurposeScript
      ( TypedMultiPurposeScript,
        certifyingTypedScript,
        mintingTypedScript,
        proposingTypedScript,
        rewardingTypedScript,
        spendingTypedScript,
        votingTypedScript
      ),
    VotingScriptType,
    mkMultiPurposeScript,
  )
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V3
  ( Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    MintValue,
    TokenName,
    TxInInfo (TxInInfo),
    TxOut (TxOut),
    Value,
    mintValueToMap,
    singleton,
  )
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
  ( Bool (False, True),
    Eq ((==)),
    Integer,
    elem,
    ($),
    (&&),
    (.),
  )
import PlutusTx.TH (compile)

-- | Default certifying scripts
trueCertifyingScript, falseCertifyingScript :: CertifyingScriptType red txInfo
trueCertifyingScript _ _ _ _ = True
falseCertifyingScript _ _ _ _ = False

-- | Default minting scripts
trueMintingScript, falseMintingScript :: MintingScriptType red txInfo
trueMintingScript _ _ _ = True
falseMintingScript _ _ _ = False

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingScript #-}
forwardingMintingScript :: (ToScriptHash script) => script -> (mtx -> [TxInInfo]) -> MintingScriptType red mtx
forwardingMintingScript (toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingScript #-}
ownForwardingMintingScript :: (mtx -> [TxInInfo]) -> MintingScriptType red mtx
ownForwardingMintingScript toTxInInfos cs = forwardingMintingScript cs toTxInInfos cs

-- | Default proposing scripts
trueProposingScript, falseProposingScript :: ProposingScriptType red txInfo
trueProposingScript _ _ _ _ = True
falseProposingScript _ _ _ _ = False

-- | Default rewarding scripts
trueRewardingScript, falseRewardingScript :: RewardingScriptType red txInfo
trueRewardingScript _ _ _ = True
falseRewardingScript _ _ _ = False

-- | Default spending scripts
trueSpendingScript, falseSpendingScript :: SpendingScriptType dat red txInfo
trueSpendingScript _ _ _ _ = True
falseSpendingScript _ _ _ _ = False

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingScript #-}
forwardingSpendingScript :: (ToScriptHash script) => script -> (txInfo -> MintValue) -> SpendingScriptType dat red txInfo
forwardingSpendingScript (toScriptHash -> ScriptHash hash) toMintValue _ _ _ (mintValueToMap . toMintValue -> mintValue) =
  CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingScript #-}
ownForwardingSpendingScript :: (txInfo -> MintValue) -> (txInfo -> [TxInInfo]) -> SpendingScriptType dat red txInfo
ownForwardingSpendingScript toMintValue toTxInInfos oRef dat red txInfo =
  case [hash | TxInInfo ref (TxOut (Address (ScriptCredential hash) _) _ _ _) <- toTxInInfos txInfo, ref == oRef] of
    [hash] -> forwardingSpendingScript hash toMintValue oRef dat red txInfo
    _ -> False

-- | Default voting scripts
trueVotingScript, falseVotingScript :: VotingScriptType red txInfo
trueVotingScript _ _ _ = True
falseVotingScript _ _ _ = False

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
trueMintingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript `withMintingPurpose` trueMintingScript @() @()

trueSpendingMPScript :: MultiPurposeScript a
trueSpendingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript $ falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingScript @() @() @()

falseMPScript :: MultiPurposeScript a
falseMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript

multiPurposeScriptValue :: MultiPurposeScript a -> TokenName -> Integer -> Value
multiPurposeScriptValue mpScript = singleton (toCurrencySymbol mpScript)
