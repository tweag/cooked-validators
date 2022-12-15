{-# LANGUAGE RecordWildCards #-}

module Cooked.MockChain.Misc where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import Data.Either
import Data.Maybe
import qualified Ledger as Pl
import qualified Ledger.Tx.CardanoAPI.Internal as Pl
import qualified Plutus.V2.Ledger.Tx as Pl (OutputDatum (..))
import qualified PlutusTx as Pl

theNetworkId :: Api.NetworkId
theNetworkId = Api.Testnet $ Api.NetworkMagic 42 -- TODO PORT what's magic?

toPlTxOut :: Pl.ToData a => Pl.Address -> Pl.Value -> Maybe a -> Pl.TxOut
toPlTxOut addr value datum = toPlTxOut' addr value datum'
  where
    datum' = maybe Pl.NoOutputDatum (Pl.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData) datum

toPlTxOut' :: Pl.Address -> Pl.Value -> Pl.OutputDatum -> Pl.TxOut
toPlTxOut' addr value datum = Pl.TxOut $ toCardanoTxOut' addr value datum

toCardanoTxOut' :: Pl.Address -> Pl.Value -> Pl.OutputDatum -> Api.TxOut Api.CtxTx Api.BabbageEra
toCardanoTxOut' addr value datum = Api.TxOut cAddr cValue cDatum Api.ReferenceScriptNone
  where
    fromRight' x = case x of
      Left err -> error $ show err
      Right res -> res
    cAddr = fromRight' $ Pl.toCardanoAddressInEra theNetworkId addr
    cValue = fromRight' $ Pl.toCardanoTxOutValue value
    cDatum = fromRight' $ Pl.toCardanoTxOutDatum datum

cTxOutToCito :: Pl.TxOut -> Maybe Pl.ChainIndexTxOut
cTxOutToCito (Pl.TxOut (Api.TxOut cAddr cValue cDatum cRefScript))
  | Just _pkh <- Pl.toPubKeyHash _ciTxOutAddress =
    Just
      Pl.PublicKeyChainIndexTxOut
        { Pl._ciTxOutPublicKeyDatum = mDatum,
          ..
        }
  | Just vHash <- Pl.toValidatorHash _ciTxOutAddress =
    Just
      Pl.ScriptChainIndexTxOut
        { Pl._ciTxOutScriptDatum = fromJust mDatum,
          Pl._ciTxOutValidator = (vHash, Nothing), -- TODO PORT can/shall we recover the full validator?
          ..
        }
  | otherwise = Nothing -- TODO PORT shall this be a hard error?
  where
    _ciTxOutAddress = Pl.fromCardanoAddressInEra cAddr
    _ciTxOutValue = Pl.fromCardanoTxOutValue cValue
    mDatum = case Pl.fromCardanoTxOutDatum cDatum of
      Pl.NoOutputDatum -> Nothing
      Pl.OutputDatumHash dh -> Just (dh, Nothing)
      Pl.OutputDatum datum -> Just (Pl.datumHash datum, Just datum)
    _ciTxOutReferenceScript = case cRefScript of
      Api.ReferenceScript _ s -> Pl.fromCardanoScriptInAnyLang s
      Api.ReferenceScriptNone -> Nothing

isPkCito :: Pl.ChainIndexTxOut -> Bool
isPkCito Pl.PublicKeyChainIndexTxOut {} = True
isPkCito _ = False

isScriptCito :: Pl.ChainIndexTxOut -> Bool
isScriptCito Pl.ScriptChainIndexTxOut {} = True
isScriptCito _ = False
