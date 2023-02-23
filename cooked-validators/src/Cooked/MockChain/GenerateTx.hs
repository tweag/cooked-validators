{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Cooked.MockChain.GenerateTx
  ( GenerateTxError (..),
    GenTxParams (gtpCollateralIns, gtpFee),
    generateTxBodyContent,
    txSkelOutToCardanoTxOut,
    generateTx,
  )
where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Node.Emulator.Params as Emulator
import Control.Arrow
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Bifunctor
import Data.Default
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, validatorHash)
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Pl
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Wallet.API as Pl

data GenerateTxError
  = ToCardanoError String Pl.ToCardanoError
  | TxBodyError String C.TxBodyError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

-- | The internal (do-not-modify unless you know what you're doing) parameters
-- for 'generateTxBodyContent'.
data GenTxParams = GenTxParams
  { -- | The collateral UTxOs to use for the transaction.
    --
    -- It is the duty of the caller to choose and set the collateral UTxOs.
    -- 'generateTxBodyContent' will not do it.
    gtpCollateralIns :: Set Pl.TxOutRef,
    -- | The transaction fee (in Lovelace)
    gtpFee :: Fee
  }

instance Default GenTxParams where
  def = GenTxParams {gtpCollateralIns = mempty, gtpFee = 0}

generateTxBodyContent ::
  -- | Parameters controlling transaction generation.
  GenTxParams ->
  -- | Some parameters, coming from the 'MockChain'.
  Pl.Params ->
  -- | All of the currently known data on transaction inputs, also coming from the 'MockChain'.
  Map Pl.DatumHash Pl.Datum ->
  -- | All of the currently known UTxOs which will be used as transaction inputs or referenced, also coming from the 'MockChain'.
  Map Pl.TxOutRef Pl.TxOut ->
  -- | All of the currently known validators which protect transaction inputs, also coming from the 'MockChain'.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  -- | The transaction skeleton to translate.
  TxSkel ->
  Either
    GenerateTxError
    (C.TxBodyContent C.BuildTx C.BabbageEra)
generateTxBodyContent GenTxParams {..} theParams managedData managedTxOuts managedValidators skel = do
  txIns <- mapM txSkelInToTxIn $ Map.toList (txSkelIns skel)
  txInsReference <- txOutRefsToTxInsReference $ Set.toList (txSkelInsReference skel)
  txInsCollateral <- txOutRefsToTxSkelInsCollateral $ Set.toList gtpCollateralIns
  txOuts <- mapM (txSkelOutToCardanoTxOut theParams) $ txSkelOuts skel
  txValidityRange <-
    left
      (ToCardanoError "translating the transaction validity range")
      . Pl.toCardanoValidityRange
      $ txSkelValidityRange skel
  txMintValue <- txSkelMintsToTxMintValue $ txSkelMints skel
  txExtraKeyWits <-
    let signers = txSkelSigners skel
     in if null signers
          then Left $ GenerateTxErrorGeneral "empty txSkelSigners. You must provide at least one signer"
          else
            bimap
              (ToCardanoError "translating the required signers")
              (C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra)
              $ mapM
                (Pl.toCardanoPaymentKeyHash . Pl.PaymentPubKeyHash . walletPKHash)
                signers
  txTotalCollateral <-
    right
      ( C.TxTotalCollateral (Maybe.fromJust (C.totalAndReturnCollateralSupportedInEra C.BabbageEra))
          . C.Lovelace
          . Pl.getLovelace
          . Pl.fromValue
          . mconcat
      )
      ( mapM
          ( \txOutRef -> do
              Pl.TxOut _ outValue _ _ <-
                throwOnNothing
                  (GenerateTxErrorGeneral $ "computing the total collateral: Unknown TxOutRef" ++ show txOutRef)
                  (Map.lookup txOutRef managedTxOuts)
              Right outValue
          )
          $ Set.toList gtpCollateralIns
      )
  Right $
    C.TxBodyContent
      { C.txIns = txIns,
        C.txInsCollateral = txInsCollateral,
        C.txInsReference = txInsReference,
        C.txOuts = txOuts,
        C.txTotalCollateral = txTotalCollateral,
        -- WARN For now we are not dealing with return collateral
        C.txReturnCollateral = C.TxReturnCollateralNone, -- That's what plutus-apps does as well
        C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra $ C.Lovelace $ feeLovelace gtpFee,
        C.txValidityRange = txValidityRange,
        C.txMetadata = C.TxMetadataNone, -- That's what plutus-apps does as well
        C.txAuxScripts = C.TxAuxScriptsNone, -- That's what plutus-apps does as well
        C.txExtraKeyWits = txExtraKeyWits,
        C.txProtocolParams = C.BuildTxWith . Just . Emulator.pProtocolParams $ theParams, -- That's what plutus-apps does as well
        C.txWithdrawals = C.TxWithdrawalsNone, -- That's what plutus-apps does as well
        C.txCertificates = C.TxCertificatesNone, -- That's what plutus-apps does as well
        C.txUpdateProposal = C.TxUpdateProposalNone, -- That's what plutus-apps does as well
        C.txMintValue = txMintValue,
        C.txScriptValidity = C.TxScriptValidityNone -- That's what plutus-apps does as well
      }
  where
    -- Helper function to throw errors.
    throwOnNothing :: e -> Maybe a -> Either e a
    throwOnNothing err = maybe (Left err) Right

    -- Convert a 'TxSkel' input, which consists of a 'Pl.TxOutRef' and a
    -- 'TxSkelIn', into a 'C.TxIn', together with the appropriate witness. If
    -- you add reference inputs, don't forget to also update the
    -- 'txInsReference'!
    txSkelInToTxIn ::
      (Pl.TxOutRef, TxSkelRedeemer) ->
      Either
        GenerateTxError
        ( C.TxIn,
          C.BuildTxWith
            C.BuildTx
            (C.Witness C.WitCtxTxIn C.BabbageEra)
        )
    txSkelInToTxIn (txOutRef, txSkelRedeemer) = do
      witness <- mkWitness txSkelRedeemer
      bimap
        (ToCardanoError "txSkelIntoTxIn, translating TxOutRef")
        (,C.BuildTxWith witness)
        $ Pl.toCardanoTxIn txOutRef
      where
        resolveScriptOutputOwnerAndDatum ::
          Either
            GenerateTxError
            ( Pl.ValidatorHash,
              Pl.Versioned Pl.Validator,
              C.ScriptDatum C.WitCtxTxIn
            )
        resolveScriptOutputOwnerAndDatum = do
          txOut <-
            throwOnNothing
              (GenerateTxErrorGeneral "txSkelInToTxIn: Unknown txOutRef")
              (Map.lookup txOutRef managedTxOuts)
          validatorHash <-
            case outputAddress txOut of
              (Pl.Address (Pl.ScriptCredential validatorHash) _) -> Right validatorHash
              _ -> Left (GenerateTxErrorGeneral "txSkelInToTxIn: Output is not a script output")
          validator <-
            throwOnNothing
              (GenerateTxErrorGeneral "txSkelInToTxIn: Unknown validator")
              (Map.lookup validatorHash managedValidators)
          datum <-
            case outputOutputDatum txOut of
              Pl.NoOutputDatum -> Left (GenerateTxErrorGeneral "txSkelInToTxIn: No datum found on script output")
              Pl.OutputDatum _datum -> Right C.InlineScriptDatum
              Pl.OutputDatumHash datumHash ->
                throwOnNothing
                  (GenerateTxErrorGeneral "txSkelInToTxIn: Datum hash could not be resolved")
                  (C.ScriptDatumForTxIn . Pl.toCardanoScriptData . Pl.getDatum <$> Map.lookup datumHash managedData)
          return (validatorHash, validator, datum)

        mkWitness :: TxSkelRedeemer -> Either GenerateTxError (C.Witness C.WitCtxTxIn C.BabbageEra)
        mkWitness TxSkelNoRedeemerForPK = Right $ C.KeyWitness C.KeyWitnessForSpending
        mkWitness (TxSkelRedeemerForReferencedScript redeemer) = do
          (validatorHash, validator, datum) <- resolveScriptOutputOwnerAndDatum
          validatorOref <-
            throwOnNothing
              (GenerateTxErrorGeneral "txSkelInToTxIn: Can't find reference script. In order to use a reference script, you must include the output where it is stored in the 'txSkelInsReference'.")
              $ find
                ( \oref -> case Map.lookup oref managedTxOuts of
                    Nothing -> False
                    Just output -> case output ^. outputReferenceScriptL of
                      Nothing -> False
                      Just scriptHash -> scriptHash == toScriptHash validator
                )
                (txSkelInsReference skel)
          validatorTxIn <-
            left
              (ToCardanoError "txSkelIntoTxIn: translating TxOutRef where the reference script sits")
              $ Pl.toCardanoTxIn validatorOref
          scriptHash <-
            left
              (ToCardanoError "txSkelInToTxIn: could not convert script hash of referenced script")
              (Pl.toCardanoScriptHash validatorHash)
          let scriptWitnessBuilder = case validator of
                Pl.Versioned _ Pl.PlutusV1 ->
                  C.PlutusScriptWitness
                    C.PlutusScriptV1InBabbage
                    C.PlutusScriptV1
                    (C.PReferenceScript validatorTxIn (Just scriptHash))
                Pl.Versioned _ Pl.PlutusV2 ->
                  C.PlutusScriptWitness
                    C.PlutusScriptV2InBabbage
                    C.PlutusScriptV2
                    (C.PReferenceScript validatorTxIn (Just scriptHash))
          return $
            C.ScriptWitness C.ScriptWitnessForSpending $
              scriptWitnessBuilder
                datum
                (Pl.toCardanoScriptData $ Pl.toBuiltinData redeemer)
                Pl.zeroExecutionUnits -- We can't guess that yet, no?
        mkWitness (TxSkelRedeemerForScript redeemer) = do
          (_validatorHash, validator, datum) <- resolveScriptOutputOwnerAndDatum
          scriptWitnessBuilder <-
            case validator of
              Pl.Versioned (Pl.Validator script) Pl.PlutusV1 ->
                bimap
                  (ToCardanoError "txSkelIntoTxIn, translating to Cardano API PlutusV1 script")
                  (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
              Pl.Versioned (Pl.Validator script) Pl.PlutusV2 ->
                bimap
                  (ToCardanoError "txSkelIntoTxIn, translating to Cardano API PlutusV2 script")
                  (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
          return $
            C.ScriptWitness C.ScriptWitnessForSpending $
              scriptWitnessBuilder
                datum
                (Pl.toCardanoScriptData $ Pl.toBuiltinData redeemer)
                Pl.zeroExecutionUnits -- We can't guess that yet, no?

    -- Convert a list of 'Pl.TxOutRef' into a 'C.TxInsReference'
    txOutRefsToTxInsReference :: [Pl.TxOutRef] -> Either GenerateTxError (C.TxInsReference C.BuildTx C.BabbageEra)
    txOutRefsToTxInsReference =
      bimap
        (ToCardanoError "txOutRefsToTxInsReference")
        ( \case
            [] -> C.TxInsReferenceNone
            txIns -> C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txIns
        )
        . mapM Pl.toCardanoTxIn

    -- Convert a list of 'Pl.TxOutRef' into a 'C.TxInsCollateral'
    txOutRefsToTxSkelInsCollateral :: [Pl.TxOutRef] -> Either GenerateTxError (C.TxInsCollateral C.BabbageEra)
    txOutRefsToTxSkelInsCollateral =
      left (ToCardanoError "txOutRefsToTxInCollateral")
        . Pl.toCardanoTxInsCollateral
        . (toPKTxInput <$>)
      where
        toPKTxInput :: Pl.TxOutRef -> Pl.TxInput
        toPKTxInput txOutRef = Pl.TxInput txOutRef Pl.TxConsumePublicKeyAddress

    -- Convert the 'TxSkelMints' into a 'TxMintValue'
    txSkelMintsToTxMintValue :: TxSkelMints -> Either GenerateTxError (C.TxMintValue C.BuildTx C.BabbageEra)
    txSkelMintsToTxMintValue mints =
      if mints == Map.empty
        then Right C.TxMintNone
        else C.TxMintValue C.MultiAssetInBabbageEra <$> mintVal <*> (C.BuildTxWith <$> witnessMap)
      where
        mintVal :: Either GenerateTxError C.Value
        mintVal =
          left (ToCardanoError "txSkelMintsToTxMintValue, translating minted value")
            . Pl.toCardanoValue
            . txSkelMintsValue
            $ mints

        witnessMap :: Either GenerateTxError (Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra))
        witnessMap =
          right mconcat $
            mapM
              ( \(policy, redeemer, _tName, _amount) ->
                  Map.singleton
                    <$> left
                      (ToCardanoError "txSkelMintsToTxMintValue, calculating the witness map")
                      (Pl.toCardanoPolicyId (Pl.mintingPolicyHash policy))
                    <*> mkMintWitness policy redeemer
              )
              $ txSkelMintsToList mints

        mkMintWitness ::
          Pl.Versioned Pl.MintingPolicy ->
          MintsRedeemer ->
          Either
            GenerateTxError
            (C.ScriptWitness C.WitCtxMint C.BabbageEra)
        mkMintWitness (Pl.Versioned (Pl.MintingPolicy script) version) redeemer = do
          scriptWitnessBuilder <-
            case version of
              Pl.PlutusV1 ->
                bimap
                  (ToCardanoError "txSkelMintsToTxMintValue, translating to Cardano API PlutusV1 script")
                  (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
              Pl.PlutusV2 ->
                bimap
                  (ToCardanoError "txSkelMintsToTxMintValue, translating to Cardano API PlutusV2 script")
                  (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
          return $
            scriptWitnessBuilder
              C.NoScriptDatumForMint -- This seems to be the only well-typed option (?)
              ( case redeemer of
                  NoMintsRedeemer -> Pl.toCardanoScriptData $ Pl.toBuiltinData () -- This is also how plutus-apps is doing it: Using no redeemer means using '()' on-chain
                  SomeMintsRedeemer red -> Pl.toCardanoScriptData $ Pl.toBuiltinData red
              )
              Pl.zeroExecutionUnits -- This is what plutus-apps does as well, we can't know this yet, no?

-- Convert a 'TxSkelOut' to the corresponding 'C.TxOut'.
txSkelOutToCardanoTxOut :: Pl.Params -> TxSkelOut -> Either GenerateTxError (C.TxOut C.CtxTx C.BabbageEra)
txSkelOutToCardanoTxOut theParams (Pays output) =
  left (ToCardanoError "txSkelOutToTxOut") $
    C.TxOut
      <$> Pl.toCardanoAddressInEra (Pl.pNetworkId theParams) (outputAddress output)
      <*> (Pl.toCardanoTxOutValue <$> Pl.toCardanoValue (outputValue output))
      <*> ( case output ^. outputDatumL of
              TxSkelOutNoDatum -> Right Pl.toCardanoTxOutNoDatum
              TxSkelOutDatumHash datum -> Pl.toCardanoTxOutDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
              TxSkelOutDatum datum -> Right . Pl.toCardanoTxOutDatumInTx . Pl.Datum . Pl.toBuiltinData $ datum
              TxSkelOutInlineDatum datum -> Right . Pl.toCardanoTxOutDatumInline . Pl.Datum . Pl.toBuiltinData $ datum
          )
      <*> Pl.toCardanoReferenceScript (toScript <$> output ^. outputReferenceScriptL)

generateTx ::
  -- | Parameters controlling transaction generation.
  GenTxParams ->
  -- | Some parameters, coming from the 'MockChain'.
  Pl.Params ->
  -- | All of the currently known data on transaction inputs, also coming from the 'MockChain'.
  Map Pl.DatumHash Pl.Datum ->
  -- | All of the currently known UTxOs which will be used as transaction inputs or referenced, also coming from the 'MockChain'.
  Map Pl.TxOutRef Pl.TxOut ->
  -- | All of the currently known validators which protect transaction inputs, also coming from the 'MockChain'.
  Map Pl.ValidatorHash (Pl.Versioned Pl.Validator) ->
  -- | The transaction skeleton to translate.
  TxSkel ->
  Either
    GenerateTxError
    (C.Tx C.BabbageEra)
generateTx genTxParams params datums txOuts validators skel = do
  txBodyContent <- generateTxBodyContent genTxParams params datums txOuts validators skel
  cardanoTxUnsigned <-
    bimap
      (TxBodyError "generateTx: ")
      (flip C.Tx [])
      -- WARN 'makeTransactionBody' will be deprecated in newer versions of
      -- cardano-api
      -- The new name (which is more fitting as well) is
      -- 'createAndValidateTransactionBody'.
      (C.makeTransactionBody txBodyContent)
  let cardanoTxSigned = foldl' txAddSignature cardanoTxUnsigned (txSkelSigners skel)
  return $ applyRawModOnBalancedTx (txOptUnsafeModTx . txSkelOpts $ skel) cardanoTxSigned
  where
    txAddSignature :: C.Tx C.BabbageEra -> Wallet -> C.Tx C.BabbageEra
    txAddSignature tx wal = case Ledger.addCardanoTxSignature
      (walletSK wal)
      (Ledger.CardanoApiTx $ Ledger.CardanoApiEmulatorEraTx tx) of
      Ledger.CardanoApiTx (Ledger.CardanoApiEmulatorEraTx tx') -> tx'
      -- Looking at the implementation of Ledger.addCardanoTxSignature:
      -- It never changes the constructor used, so the above branch
      -- will never happen
      _ -> error "generateTx: expected CardanoApiTx"
