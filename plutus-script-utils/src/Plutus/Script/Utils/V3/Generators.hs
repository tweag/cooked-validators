{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V3.Generators
  ( trueCertifyingPurpose,
    trueMintingPurpose,
    forwardingMintingPurpose,
    ownForwardingMintingPurpose,
    trueProposingPurpose,
    trueRewardingPurpose,
    trueSpendingPurpose,
    forwardingSpendingPurpose,
    ownForwardingSpendingPurpose,
    trueVotingPurpose,
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

import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Plutus.Script.Utils.Scripts
  ( ScriptHash (ScriptHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    toCurrencySymbol,
  )
import Plutus.Script.Utils.V3.Typed
  ( CertifyingPurposeType',
    MintingPurposeType',
    MultiPurposeScript (MultiPurposeScript),
    ProposingPurposeType',
    RewardingPurposeType',
    SpendingPurposeType',
    TypedMultiPurposeScript
      ( TypedMultiPurposeScript,
        certifyingPurpose,
        mintingPurpose,
        proposingPurpose,
        rewardingPurpose,
        spendingPurpose,
        votingPurpose
      ),
    VotingPurposeType',
    mkMultiPurposeScript,
  )
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V3
  ( Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    FromData,
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

trueCertifyingPurpose :: CertifyingPurposeType' red txInfo
trueCertifyingPurpose _ _ _ _ = True

trueMintingPurpose :: MintingPurposeType' red txInfo
trueMintingPurpose _ _ _ = True

-- | Minting script that ensures a given spending script is invoked in the transaction
{-# INLINEABLE forwardingMintingPurpose #-}
forwardingMintingPurpose :: (ToScriptHash script) => script -> (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
forwardingMintingPurpose (toScriptHash -> sHash) toTxInInfos _ _ (toTxInInfos -> txInInfos) =
  sHash `elem` [h | TxInInfo _ (TxOut (Address (ScriptCredential h) _) _ _ _) <- txInInfos]

-- | Minting policy that ensures the own spending script is invoked in the transaction
{-# INLINEABLE ownForwardingMintingPurpose #-}
ownForwardingMintingPurpose :: (mtx -> [TxInInfo]) -> MintingPurposeType' red mtx
ownForwardingMintingPurpose toTxInInfos cs = forwardingMintingPurpose cs toTxInInfos cs

trueProposingPurpose :: ProposingPurposeType' red txInfo
trueProposingPurpose _ _ _ _ = True

trueRewardingPurpose :: RewardingPurposeType' red txInfo
trueRewardingPurpose _ _ _ = True

trueSpendingPurpose :: SpendingPurposeType' dat red txInfo
trueSpendingPurpose _ _ _ _ = True

-- | Spending script that ensures a given minting script is invoked in the transaction
{-# INLINEABLE forwardingSpendingPurpose #-}
forwardingSpendingPurpose :: (ToScriptHash script) => script -> (txInfo -> MintValue) -> SpendingPurposeType' dat red txInfo
forwardingSpendingPurpose (toScriptHash -> ScriptHash hash) toMintValue _ _ _ (mintValueToMap . toMintValue -> mintValue) =
  CurrencySymbol hash `Map.member` mintValue

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE ownForwardingSpendingPurpose #-}
ownForwardingSpendingPurpose :: (txInfo -> MintValue) -> (txInfo -> [TxInInfo]) -> SpendingPurposeType' dat red txInfo
ownForwardingSpendingPurpose toMintValue toTxInInfos oRef dat red txInfo =
  case [hash | TxInInfo ref (TxOut (Address (ScriptCredential hash) _) _ _ _) <- toTxInInfos txInfo, ref == oRef] of
    [hash] -> forwardingSpendingPurpose hash toMintValue oRef dat red txInfo
    _ -> False

trueVotingPurpose :: VotingPurposeType' red txInfo
trueVotingPurpose _ _ _ = True

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
  @falseTypedMultiPurposeScript `withSpendingPurpose` trueSpendingPurpose@

--}

falseTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () ()
falseTypedMultiPurposeScript =
  TypedMultiPurposeScript
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

trueTypedMultiPurposeScript :: TypedMultiPurposeScript () () () () () () () () () () () () ()
trueTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (Just trueCertifyingPurpose)
    (Just trueMintingPurpose)
    (Just trueProposingPurpose)
    (Just trueRewardingPurpose)
    (Just trueSpendingPurpose)
    (Just trueVotingPurpose)

-- | Overrides the certifying purpose
withCertifyingPurpose ::
  (FromData cr', FromData ctx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  CertifyingPurposeType' cr' ctx' ->
  TypedMultiPurposeScript cr' ctx' mr mtx pr ptx rr rtx sd sr stx vr vtx
withCertifyingPurpose ts cs = ts {certifyingPurpose = Just cs}

-- | Combines a new certifying purpose with the existing one
addCertifyingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  CertifyingPurposeType' cr ctx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addCertifyingPurpose ts@(TypedMultiPurposeScript {certifyingPurpose}) cs =
  ts `withCertifyingPurpose` \ix cert red txInfo ->
    fromMaybe trueCertifyingPurpose certifyingPurpose ix cert red txInfo
      && cs ix cert red txInfo

-- | Overrides the certifying purpose
withMintingPurpose ::
  (FromData mr', FromData mtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  MintingPurposeType' mr' mtx' ->
  TypedMultiPurposeScript cr ctx mr' mtx' pr ptx rr rtx sd sr stx vr vtx
withMintingPurpose ts ms = ts {mintingPurpose = Just ms}

-- | Combines a new minting purpose with the existing one
addMintingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  MintingPurposeType' mr mtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addMintingPurpose ts@(TypedMultiPurposeScript {mintingPurpose}) ms =
  ts `withMintingPurpose` \cs red txInfo ->
    fromMaybe trueMintingPurpose mintingPurpose cs red txInfo
      && ms cs red txInfo

-- | Overrides the proposing purpose
withProposingPurpose ::
  (FromData pr', FromData ptx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  ProposingPurposeType' pr' ptx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr' ptx' rr rtx sd sr stx vr vtx
withProposingPurpose ts ps = ts {proposingPurpose = Just ps}

-- | Combines a new proposing purpose with the existing one
addProposingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  ProposingPurposeType' pr ptx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addProposingPurpose ts@(TypedMultiPurposeScript {proposingPurpose}) ps =
  ts `withProposingPurpose` \ix prop red txInfo ->
    fromMaybe trueProposingPurpose proposingPurpose ix prop red txInfo
      && ps ix prop red txInfo

-- | Overrides the rewardings purpose
withRewardingPurpose ::
  (FromData rr', FromData rtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  RewardingPurposeType' rr' rtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr' rtx' sd sr stx vr vtx
withRewardingPurpose ts rs = ts {rewardingPurpose = Just rs}

-- | Combines a new rewardings purpose with the existing one
addRewardingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  RewardingPurposeType' rr rtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addRewardingPurpose ts@(TypedMultiPurposeScript {rewardingPurpose}) rs =
  ts `withRewardingPurpose` \cred red txInfo ->
    fromMaybe trueRewardingPurpose rewardingPurpose cred red txInfo
      && rs cred red txInfo

-- | Overrides the spending purpose
withSpendingPurpose ::
  (FromData sd', FromData sr', FromData stx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  SpendingPurposeType' sd' sr' stx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd' sr' stx' vr vtx
withSpendingPurpose ts ss = ts {spendingPurpose = Just ss}

-- | Combines a new spending purpose with the existing one
addSpendingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  SpendingPurposeType' sd sr stx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addSpendingPurpose ts@(TypedMultiPurposeScript {spendingPurpose}) ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo ->
    fromMaybe trueSpendingPurpose spendingPurpose oRef mDat red txInfo
      && ss oRef mDat red txInfo

-- | Overrides the voting purpose
withVotingPurpose ::
  (FromData vr', FromData vtx') =>
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  VotingPurposeType' vr' vtx' ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr' vtx'
withVotingPurpose ts vs = ts {votingPurpose = Just vs}

-- | Combines a new voting purpose with the existing one
addVotingPurpose ::
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx ->
  VotingPurposeType' vr vtx ->
  TypedMultiPurposeScript cr ctx mr mtx pr ptx rr rtx sd sr stx vr vtx
addVotingPurpose ts@(TypedMultiPurposeScript {votingPurpose}) vs =
  ts `withVotingPurpose` \voter red txInfo ->
    fromMaybe trueVotingPurpose votingPurpose voter red txInfo
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

-- | The multi-purpose script that returns @True@ in minting purpose and @False@
-- otherwise
trueMintingMPScript :: MultiPurposeScript a
trueMintingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript
        $ falseTypedMultiPurposeScript
        `withMintingPurpose` (trueMintingPurpose @() @())

-- | The multi-purpose script that returns @True@ in spending purpose and @False@
-- otherwise
trueSpendingMPScript :: MultiPurposeScript a
trueSpendingMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script =
      mkMultiPurposeScript
        $ falseTypedMultiPurposeScript
        `withSpendingPurpose` trueSpendingPurpose @() @() @()

falseMPScript :: MultiPurposeScript a
falseMPScript = MultiPurposeScript $ toScript $$(compile [||script||])
  where
    script = mkMultiPurposeScript falseTypedMultiPurposeScript

multiPurposeScriptValue :: MultiPurposeScript a -> TokenName -> Integer -> Value
multiPurposeScriptValue mpScript = singleton (toCurrencySymbol mpScript)
