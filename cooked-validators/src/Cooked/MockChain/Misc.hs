{-# LANGUAGE RecordWildCards #-}

module Cooked.MockChain.Misc where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import Data.Maybe
import qualified Ledger as Pl
import qualified Ledger.Tx.CardanoAPI.Internal as Pl
import qualified Plutus.V2.Ledger.Tx as Pl (OutputDatum(..))
import qualified Plutus.Script.Utils.V2.Scripts as Pl

theNetworkId :: Api.NetworkId
theNetworkId = Api.Testnet $ Api.NetworkMagic 42 -- TODO PORT what's magic?

fromTxOut :: Pl.TxOut -> Maybe Pl.ChainIndexTxOut
fromTxOut (Pl.TxOut (Api.TxOut cAddr cValue cDatum cRefScript))
  | Just _pkh <- Pl.toPubKeyHash _ciTxOutAddress
    = Just Pl.PublicKeyChainIndexTxOut
        { Pl._ciTxOutPublicKeyDatum = mDatum
        , ..
        }
  | Just vHash <- Pl.toValidatorHash _ciTxOutAddress
    = Just Pl.ScriptChainIndexTxOut
        { Pl._ciTxOutScriptDatum = fromJust mDatum
        , Pl._ciTxOutValidator = (vHash, Nothing)       -- TODO PORT can/shall we recover the full validator?
        , ..
        }
  | otherwise = Nothing
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
