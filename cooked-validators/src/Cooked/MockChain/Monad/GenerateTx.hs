{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cooked.MockChain.Monad.GenerateTx where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Control.Arrow
import Cooked.Tx.Constraints.Type
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (TxOut, validatorHash)
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core
import qualified Plutus.V2.Ledger.Api as Pl

data GenerateTxError
  = ToCardanoError String Pl.ToCardanoError
  | GenerateTxErrorGeneral String
  deriving (Show, Eq)

generateTxBodyContent ::
  Pl.Params ->
  Map Pl.DatumHash Pl.Datum ->
  TxSkel ->
  Either
    GenerateTxError
    (C.TxBodyContent C.BuildTx C.BabbageEra)
generateTxBodyContent theParams managedData skel = do
  txIns <- mapM inConstraintToTxIn $ Set.toList (skel ^. txSkelIns)
  txInsCollateral <- spOutsToTxInsCollateral . Set.toList $ skel ^. txSkelInsCollateral
  txOuts <- mapM outConstraintToTxOut $ skel ^. txSkelOuts
  txValidityRange <-
    left
      (ToCardanoError "translating the transaction validity range")
      . Pl.toCardanoValidityRange
      . Pl.posixTimeRangeToContainedSlotRange (Pl.pSlotConfig theParams)
      $ skel ^. txSkelValidityRange
  txMintValue <- txSkelMintsToTxMintValue $ skel ^. txSkelMints
  Right $
    C.TxBodyContent
      { C.txIns = txIns,
        C.txInsCollateral = txInsCollateral,
        -- We don't yet support reference inputs. If you add this
        -- functionality, remember that both the 'txIns' and 'txInsReference'
        -- fields have to change!
        C.txInsReference = C.TxInsReferenceNone,
        C.txOuts = txOuts,
        C.txTotalCollateral = C.TxTotalCollateralNone, -- That's what plutus-apps does as well
        C.txReturnCollateral = C.TxReturnCollateralNone, -- That's what plutus-apps does as well
        C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra mempty, -- That's what plutus-apps does as well
        C.txValidityRange = txValidityRange,
        C.txMetadata = C.TxMetadataNone, -- That's what plutus-apps does as well
        C.txAuxScripts = C.TxAuxScriptsNone, -- That's what plutus-apps does as well
        C.txExtraKeyWits = C.TxExtraKeyWitnessesNone, -- That's what plutus-apps does as well
        C.txProtocolParams = C.BuildTxWith . Just . Pl.pProtocolParams $ theParams, -- That's what plutus-apps does as well
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

    -- This map should contain the data for all datum hashes on inputs of the
    -- transaction. This means that the data corresponding to some of the datum
    -- hashes are derived from the 'managedData'.
    inputData :: Map Pl.DatumHash Pl.Datum
    inputData =
      foldMapOf
        (txSkelIns % folded % input % spOutDatumOrHash)
        ( \(datumHash, mDatum) ->
            case mDatum of
              Just datum -> Map.singleton datumHash datum
              Nothing ->
                maybe
                  Map.empty -- throw an error here? I've decided for now to throw errors if the datum is actually needed later on.
                  (Map.singleton datumHash)
                  (Map.lookup datumHash managedData)
        )
        skel

    -- Convert an 'InConstraint' into a 'C.TxIn', together with the
    -- appropriate witness. If you add reference inputs, don't forget to also
    -- update the 'txInsReference'!
    inConstraintToTxIn ::
      InConstraint ->
      Either
        GenerateTxError
        ( C.TxIn,
          C.BuildTxWith
            C.BuildTx
            (C.Witness C.WitCtxTxIn C.BabbageEra)
        )
    inConstraintToTxIn (SpendsPK (SpendableOut txOutRef _)) =
      bimap
        (ToCardanoError "inConstraintToTxIn, translating 'SpendsPK' outRef")
        (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)
        $ Pl.toCardanoTxIn txOutRef
    inConstraintToTxIn (SpendsScript validator redeemer spOut@(SpendableOut txOutRef _)) = do
      witness <- mkWitness
      bimap
        (ToCardanoError "inConstraintToTxIn, translating 'SpendsScript' outRef")
        (,C.BuildTxWith witness)
        $ Pl.toCardanoTxIn txOutRef
      where
        mkWitness :: Either GenerateTxError (C.Witness C.WitCtxTxIn C.BabbageEra)
        mkWitness = do
          scriptWitnessBuilder <-
            case Pl.vValidatorScript validator of
              Pl.Versioned (Pl.Validator script) Pl.PlutusV1 ->
                bimap
                  (ToCardanoError "inConstraintToTxIn, translating to Cardano API PlutusV1 script")
                  (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
              Pl.Versioned (Pl.Validator script) Pl.PlutusV2 ->
                bimap
                  (ToCardanoError "inConstraintToTxIn, translating to Cardano API PlutusV2 script")
                  (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 . C.PScript)
                  (Pl.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script)
          datumHash <-
            throwOnNothing (GenerateTxErrorGeneral "inConstraintToTxIn: No datum hash on script input") $
              spOut ^? (spOutScriptDatumOrHash % _1)
          datum <-
            throwOnNothing
              (GenerateTxErrorGeneral "inConstraintToTxIn: Unknown datum hash on script input")
              (C.ScriptDatumForTxIn . Pl.toCardanoScriptData . Pl.toBuiltinData <$> inputData Map.!? datumHash)
          return $
            C.ScriptWitness C.ScriptWitnessForSpending $
              scriptWitnessBuilder
                datum
                (Pl.toCardanoScriptData $ Pl.toBuiltinData redeemer)
                Pl.zeroExecutionUnits -- We can't guess that yet, no?

    -- Convert a list of 'SpendableOut' into a 'C.TxInsCollateral'
    spOutsToTxInsCollateral :: [SpendableOut] -> Either GenerateTxError (C.TxInsCollateral C.BabbageEra)
    spOutsToTxInsCollateral =
      left (ToCardanoError "spOutsToTxInCollateral")
        . Pl.toCardanoTxInsCollateral
        . (toPKTxInput <$>)
      where
        toPKTxInput :: SpendableOut -> Pl.TxInput
        toPKTxInput o = Pl.TxInput (o ^. spOutTxOutRef) Pl.TxConsumePublicKeyAddress

    outConstraintToTxOut :: OutConstraint -> Either GenerateTxError (C.TxOut C.CtxTx C.BabbageEra)
    outConstraintToTxOut = \case
      (PaysPK pkh mStPkh mDatum value) ->
        left
          (ToCardanoError "outConstraintToTxOut, translating 'PaysPK'")
          ( Pl.toCardanoTxOut
              (Pl.pNetworkId theParams)
              Pl.toCardanoTxOutDatum
              $ Pl.TxOut
                ( Pl.Address
                    (Pl.PubKeyCredential pkh)
                    (Pl.StakingHash . Pl.PubKeyCredential . Pl.unStakePubKeyHash <$> mStPkh)
                )
                value
                (maybe Pl.NoOutputDatum (Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData) mDatum) -- What to do if we want to use only the datum hash?
                Nothing -- What to do about reference scripts?
          )
      (PaysScript validator mStCred datum value) ->
        left
          (ToCardanoError "outConstraintToTxOut, translating 'PaysScript'")
          ( Pl.toCardanoTxOut
              (Pl.pNetworkId theParams)
              Pl.toCardanoTxOutDatum
              $ Pl.TxOut
                ( Pl.Address
                    (Pl.ScriptCredential $ Pl.validatorHash validator)
                    mStCred
                )
                value
                (Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData $ datum) -- What to do if we want to use only the datum hash?
                Nothing -- What to do about reference scripts?
          )

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
