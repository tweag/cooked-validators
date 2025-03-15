{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Node.Emulator.LogMessages where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Internal.Node.Chain (ChainEvent)
import Control.Lens.TH (makePrisms)
import Data.Aeson (Value)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger (CardanoAddress, CardanoTx, getCardanoTxId, toPlutusAddress)
import Ledger.Index (UtxoIndex, ValidationError, ValidationPhase)
import Ledger.Tx.CardanoAPI (CardanoBuildTx)
import Prettyprinter (Pretty (pretty), colon, hang, viaShow, vsep, (<+>))

data EmulatorMsg
  = GenericMsg Value
  | TxBalanceMsg TxBalanceMsg
  | ChainEvent ChainEvent
  deriving stock (Eq, Show, Generic)

instance Pretty EmulatorMsg where
  pretty = \case
    GenericMsg json -> viaShow json
    TxBalanceMsg msg -> pretty msg
    ChainEvent msg -> pretty msg

data TxBalanceMsg
  = BalancingUnbalancedTx CardanoBuildTx UtxoIndex
  | -- | Stores the wallet address for fees
    FinishedBalancing CardanoTx CardanoAddress
  | SigningTx CardanoTx
  | SubmittingTx CardanoTx
  | -- | The amount of collateral stored in the transaction.
    ValidationFailed
      ValidationPhase
      CardanoTx
      ValidationError
      C.Value
  deriving stock (Eq, Show, Generic)

instance Pretty TxBalanceMsg where
  pretty = \case
    BalancingUnbalancedTx tx (C.UTxO utxo) ->
      hang 2 $
        vsep
          [ hang 2 $ vsep ["Balancing an unbalanced transaction:", pretty tx],
            hang 2 $ vsep $ "Utxo index:" : (pretty <$> Map.toList utxo)
          ]
    FinishedBalancing tx addr -> hang 2 $ vsep ["Finished balancing (using" <+> pretty (toPlutusAddress addr) <> "):", pretty tx]
    SigningTx tx -> "Signing tx:" <+> pretty (getCardanoTxId tx)
    SubmittingTx tx -> "Submitting tx:" <+> pretty (getCardanoTxId tx)
    ValidationFailed p tx e _ -> "Validation error:" <+> pretty p <+> pretty (getCardanoTxId tx) <> colon <+> pretty e

makePrisms ''TxBalanceMsg
