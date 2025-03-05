{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Optics.Core qualified as Optics
import Plutus.Script.Utils.Scripts qualified as PSU
import PlutusLedgerApi.V1.Address qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.Prelude
import PlutusTx.TH qualified as PlutusTx
import Prettyprinter qualified as PP
import Prettyprinter.Extras qualified as PP
import Prelude qualified as HS

class ValidatorTypes a where
  -- Minting purpose type variables with default
  type MintingRedeemerType a
  type MintingTxInfo a

  type MintingRedeemerType a = ()
  type MintingTxInfo a = Api.TxInfo

  -- Spending purpose type variables with default
  type SpendingRedeemerType a
  type SpendingTxInfo a
  type DatumType a

  type SpendingRedeemerType a = ()
  type SpendingTxInfo a = Api.TxInfo
  type DatumType a = ()

  -- Rewarding purpose type variables with default
  type RewardingRedeemerType a
  type RewardingTxInfo a

  type RewardingRedeemerType a = ()
  type RewardingTxInfo a = Api.TxInfo

  -- Certifying purpose type variables with default
  type CertifyingRedeemerType a
  type CertifyingTxInfo a

  type CertifyingRedeemerType a = ()
  type CertifyingTxInfo a = Api.TxInfo

  -- Voting purpose type variables with default
  type VotingRedeemerType a
  type VotingTxInfo a

  type VotingRedeemerType a = ()
  type VotingTxInfo a = Api.TxInfo

  -- Proposing purpose type variables with default
  type ProposingRedeemerType a
  type ProposingTxInfo a

  type ProposingRedeemerType a = ()
  type ProposingTxInfo a = Api.TxInfo

instance ValidatorTypes ()

type MintingScriptType a = Api.CurrencySymbol -> MintingRedeemerType a -> MintingTxInfo a -> Bool

type SpendingScriptType a = Api.TxOutRef -> Maybe (DatumType a) -> SpendingRedeemerType a -> SpendingTxInfo a -> Bool

type RewardingScriptType a = Api.Credential -> RewardingRedeemerType a -> RewardingTxInfo a -> Bool

type CertifyingScriptType a = Integer -> Api.TxCert -> CertifyingRedeemerType a -> CertifyingTxInfo a -> Bool

type VotingScriptType a = Api.Voter -> VotingRedeemerType a -> VotingTxInfo a -> Bool

type ProposingScriptType a = Integer -> Api.ProposalProcedure -> ProposingRedeemerType a -> ProposingTxInfo a -> Bool

data TypedMultiPurposeScript a = TypedMultiPurposeScript
  { mintingTypedScript :: MintingScriptType a,
    spendingTypedScript :: SpendingScriptType a,
    rewardingTypedScript :: RewardingScriptType a,
    certifyingTypedScript :: CertifyingScriptType a,
    votingTypedScript :: VotingScriptType a,
    proposingTypedScript :: ProposingScriptType a
  }

{-# INLINEABLE alwaysFalseTypedMultiPurposeScript #-}
alwaysFalseTypedMultiPurposeScript :: TypedMultiPurposeScript a
alwaysFalseTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)
    (\_ _ _ -> False)
    (\_ _ _ _ -> False)

{-# INLINEABLE alwaysTrueTypedMultiPurposeScript #-}
alwaysTrueTypedMultiPurposeScript :: TypedMultiPurposeScript a
alwaysTrueTypedMultiPurposeScript =
  TypedMultiPurposeScript
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)
    (\_ _ _ -> True)
    (\_ _ _ _ -> True)

{-# INLINEABLE alwaysTrueTypedMintingScript #-}
alwaysTrueTypedMintingScript :: TypedMultiPurposeScript a
alwaysTrueTypedMintingScript = alwaysFalseTypedMultiPurposeScript `withMintingPurpose` \_ _ _ -> True

{-# INLINEABLE alwaysTrueTypedSpendingScript #-}
alwaysTrueTypedSpendingScript :: TypedMultiPurposeScript a
alwaysTrueTypedSpendingScript = alwaysFalseTypedMultiPurposeScript `withSpendingPurpose` \_ _ _ _ -> True

-- * Working with the Minting purpose of a multipurpose script

-- | Adds (or overrides) the minting purpose to a V3 typed script
{-# INLINEABLE withMintingPurpose #-}
withMintingPurpose :: TypedMultiPurposeScript a -> MintingScriptType a -> TypedMultiPurposeScript a
withMintingPurpose ts ms = ts {mintingTypedScript = ms}

-- | Adds a minting constraint to an existing minting purpose
{-# INLINEABLE addMintingConstraint #-}
addMintingConstraint ::
  TypedMultiPurposeScript a -> MintingScriptType a -> TypedMultiPurposeScript a
addMintingConstraint ts ms = ts `withMintingPurpose` \cs red txInfo -> mintingTypedScript ts cs red txInfo && ms cs red txInfo

-- | Utility function to check that an input exists at a given script address
{-# INLINEABLE inputExistsAtScriptAddress #-}
inputExistsAtScriptAddress :: [Api.TxInInfo] -> BuiltinByteString -> Bool
inputExistsAtScriptAddress txInfo bs =
  bs
    `elem` [ h
             | Api.TxInInfo _ (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfo
           ]

-- | Getting the inputs from a TxInfo
{-# INLINEABLE txInfoInputsG #-}
txInfoInputsG :: Optics.Getter Api.TxInfo [Api.TxInInfo]
txInfoInputsG = Optics.to Api.txInfoInputs

-- | Minting policy that ensure a given validator is called in the transaction
{-# INLINEABLE withForwardingMintingScript #-}
withForwardingMintingScript ::
  TypedMultiPurposeScript a ->
  PSU.ValidatorHash ->
  Optics.Getter (MintingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withForwardingMintingScript ts (PSU.ValidatorHash hash) getter =
  ts `withMintingPurpose` \_ _ txInfo -> inputExistsAtScriptAddress (Optics.view getter txInfo) hash

-- | Minting policy that ensure the own spending purpose check is called in the transaction
{-# INLINEABLE withOwnForwardingMintingScript #-}
withOwnForwardingMintingScript ::
  TypedMultiPurposeScript a ->
  Optics.Getter (MintingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withOwnForwardingMintingScript ts getter =
  ts `withMintingPurpose` \(Api.CurrencySymbol cs) _ txInfo -> inputExistsAtScriptAddress (Optics.view getter txInfo) cs

-- * Working with the Spending purpose of a multipurpose script

-- | Adds (or overrides) the spending purpose to a V3 typed script
{-# INLINEABLE withSpendingPurpose #-}
withSpendingPurpose ::
  TypedMultiPurposeScript a -> SpendingScriptType a -> TypedMultiPurposeScript a
withSpendingPurpose ts ss = ts {spendingTypedScript = ss}

-- | Adds a spending constraint to an existing spending purpose
{-# INLINEABLE addSpendingConstraint #-}
addSpendingConstraint ::
  TypedMultiPurposeScript a -> SpendingScriptType a -> TypedMultiPurposeScript a
addSpendingConstraint ts ss =
  ts `withSpendingPurpose` \oRef mDat red txInfo -> spendingTypedScript ts oRef mDat red txInfo && ss oRef mDat red txInfo

-- | Getting the minted value from a TxInfo
{-# INLINEABLE txInfoMintValueG #-}
txInfoMintValueG :: Optics.Getter Api.TxInfo Api.Value
txInfoMintValueG = Optics.to Api.txInfoMint

-- | Spending purpose that ensures a given minting script is invoked in the transaction
{-# INLINEABLE withForwardSpendingScript #-}
withForwardSpendingScript ::
  TypedMultiPurposeScript a ->
  PSU.MintingPolicyHash ->
  Optics.Getter (SpendingTxInfo a) Api.Value ->
  TypedMultiPurposeScript a
withForwardSpendingScript ts (PSU.MintingPolicyHash hash) getter =
  ts `withSpendingPurpose` \_ _ _ txInfo -> Api.CurrencySymbol hash `Map.member` Api.getValue (Optics.view getter txInfo)

-- | Spending purpose that ensures the own minting purpose is invoked in the transaction
{-# INLINEABLE withOwnForwardSpendingScript #-}
withOwnForwardSpendingScript ::
  TypedMultiPurposeScript a ->
  Optics.Getter (SpendingTxInfo a) Api.Value ->
  Optics.Getter (SpendingTxInfo a) [Api.TxInInfo] ->
  TypedMultiPurposeScript a
withOwnForwardSpendingScript ts mintedValueGetter inputsGetter =
  ts `withSpendingPurpose` \oRef _ _ txInfo ->
    any
      ((`Map.member` Api.getValue (Optics.view mintedValueGetter txInfo)) . Api.CurrencySymbol)
      [ h
        | Api.TxInInfo ref (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <-
            Optics.view inputsGetter txInfo,
          ref == oRef
      ]

-- * Working with the Rewarding purpose of a multipurpose script

{-# INLINEABLE withRewardingPurpose #-}
withRewardingPurpose ::
  TypedMultiPurposeScript a -> RewardingScriptType a -> TypedMultiPurposeScript a
withRewardingPurpose ts rs = ts {rewardingTypedScript = rs}

{-# INLINEABLE withCertifyingPurpose #-}
withCertifyingPurpose ::
  TypedMultiPurposeScript a -> CertifyingScriptType a -> TypedMultiPurposeScript a
withCertifyingPurpose ts cs = ts {certifyingTypedScript = cs}

{-# INLINEABLE withVotingPurpose #-}
withVotingPurpose :: TypedMultiPurposeScript a -> VotingScriptType a -> TypedMultiPurposeScript a
withVotingPurpose ts vs = ts {votingTypedScript = vs}

{-# INLINEABLE withProposingPurpose #-}
withProposingPurpose ::
  TypedMultiPurposeScript a -> ProposingScriptType a -> TypedMultiPurposeScript a
withProposingPurpose ts ps = ts {proposingTypedScript = ps}

data ScriptContextResolvedScriptInfo = ScriptContextResolvedScriptInfo
  { scriptContextTxInfo :: BuiltinData,
    scriptContextRedeemer :: BuiltinData,
    scriptContextScriptInfo :: Api.ScriptInfo
  }

PlutusTx.unstableMakeIsData ''ScriptContextResolvedScriptInfo

type TypedMultiPurposeScriptConstraints a =
  ( ValidatorTypes a,
    PlutusTx.FromData (MintingRedeemerType a),
    PlutusTx.FromData (MintingTxInfo a),
    PlutusTx.FromData (SpendingRedeemerType a),
    PlutusTx.FromData (SpendingTxInfo a),
    PlutusTx.FromData (DatumType a),
    PlutusTx.FromData (RewardingRedeemerType a),
    PlutusTx.FromData (RewardingTxInfo a),
    PlutusTx.FromData (CertifyingRedeemerType a),
    PlutusTx.FromData (CertifyingTxInfo a),
    PlutusTx.FromData (VotingRedeemerType a),
    PlutusTx.FromData (VotingTxInfo a),
    PlutusTx.FromData (ProposingRedeemerType a),
    PlutusTx.FromData (ProposingTxInfo a)
  )

newtype MultiPurposeScript a = MultiPurposeScript {getMultiPurposeScript :: PSU.Script}
  deriving stock (Generic)
  deriving newtype (HS.Eq, HS.Ord, Serialise)
  deriving (PP.Pretty) via (PP.PrettyShow (MultiPurposeScript a))

instance HS.Show (MultiPurposeScript a) where
  show _ = "Multi purpose script { <script> }"

instance PSU.ToScript (MultiPurposeScript a) where
  toScript = getMultiPurposeScript

instance PSU.ToVersioned PSU.Script (MultiPurposeScript a) where
  toVersioned = (`PSU.Versioned` PSU.PlutusV3) . PSU.toScript

type UntypedMultiPurposeScript = BuiltinData -> PlutusTx.BuiltinUnit

compileUntypedMultiPurposeScript :: UntypedMultiPurposeScript -> MultiPurposeScript a
compileUntypedMultiPurposeScript script = MultiPurposeScript $ PSU.Script $ Api.serialiseCompiledCode $$(PlutusTx.compile [||script||])

alwaysTrueMintingScript :: (TypedMultiPurposeScriptConstraints a) => MultiPurposeScript a
alwaysTrueMintingScript = compileTypedMultiPurposeScript alwaysTrueTypedMintingScript

{-# INLINEABLE typedToUntypedMultiPurposeScript #-}
typedToUntypedMultiPurposeScript ::
  (TypedMultiPurposeScriptConstraints a) => TypedMultiPurposeScript a -> UntypedMultiPurposeScript
typedToUntypedMultiPurposeScript TypedMultiPurposeScript {..} dat = either traceError check $ do
  ScriptContextResolvedScriptInfo {..} <- fromBuiltinDataEither "script info" dat
  case scriptContextScriptInfo of
    Api.MintingScript cur -> do
      (red, txInfo) <- deserializeContext "minting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Minting" $ mintingTypedScript cur red txInfo
    Api.SpendingScript oRef mDat -> do
      (red, txInfo) <- deserializeContext "spending" scriptContextRedeemer scriptContextTxInfo
      mResolvedDat <- case mDat of
        Nothing -> return Nothing
        Just (Api.Datum bDat) -> Just <$> fromBuiltinDataEither "datum" bDat
      return $ traceRunning "Spending" $ spendingTypedScript oRef mResolvedDat red txInfo
    Api.RewardingScript cred -> do
      (red, txInfo) <- deserializeContext "rewarding" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Rewarding" $ rewardingTypedScript cred red txInfo
    Api.CertifyingScript i cert -> do
      (red, txInfo) <- deserializeContext "certifying" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Certifying" $ certifyingTypedScript i cert red txInfo
    Api.VotingScript voter -> do
      (red, txInfo) <- deserializeContext "voting" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Voting" $ votingTypedScript voter red txInfo
    Api.ProposingScript i prop -> do
      (red, txInfo) <- deserializeContext "proposing" scriptContextRedeemer scriptContextTxInfo
      return $ traceRunning "Proposing" $ proposingTypedScript i prop red txInfo
  where
    fromBuiltinDataEither name = maybe (Left $ "Error when deserializing the " <> name) Right . PlutusTx.fromBuiltinData
    traceRunning name = trace ("Running the validator with the " <> name <> " script purpose")
    deserializeContext name redData txInfoData = do
      red <- fromBuiltinDataEither (name <> " redeemer") redData
      txInfo <- fromBuiltinDataEither (name <> " tx info") txInfoData
      return (red, txInfo)

compileTypedMultiPurposeScript :: (TypedMultiPurposeScriptConstraints a) => TypedMultiPurposeScript a -> MultiPurposeScript a
compileTypedMultiPurposeScript = compileUntypedMultiPurposeScript . typedToUntypedMultiPurposeScript

class ToBuiltinUnit a where
  toBuiltinUnit :: a -> PlutusTx.BuiltinUnit

instance ToBuiltinUnit PlutusTx.BuiltinUnit where
  {-# INLINEABLE toBuiltinUnit #-}
  toBuiltinUnit = id

instance ToBuiltinUnit () where
  {-# INLINEABLE toBuiltinUnit #-}
  toBuiltinUnit = PlutusTx.BuiltinUnit

instance ToBuiltinUnit Bool where
  {-# INLINEABLE toBuiltinUnit #-}
  toBuiltinUnit = check

{-# INLINEABLE genericToUntypedMultiPurposeScript #-}
genericToUntypedMultiPurposeScript :: (PlutusTx.FromData ctx, ToBuiltinUnit un) => (ctx -> un) -> UntypedMultiPurposeScript
genericToUntypedMultiPurposeScript script dat = case PlutusTx.fromBuiltinData dat of
  Nothing -> traceError "Unable to deserialize to the desired type"
  Just ctx -> toBuiltinUnit $ script ctx

compileGenericMultiPurposeScript :: (PlutusTx.FromData ctx, ToBuiltinUnit un) => (ctx -> un) -> MultiPurposeScript a
compileGenericMultiPurposeScript = compileUntypedMultiPurposeScript . genericToUntypedMultiPurposeScript
