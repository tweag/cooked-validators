{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- This module is essentially a copy of
--
-- https://github.com/input-output-hk/plutus-apps/blob/e8688b8f86a92b285e7d93eb418ccc314ad41bf9/doc/plutus/tutorials/BasicApps.hs
--
-- just using cooked-validators to write the offchain code.

module Cooked.MockChain.ContractSpec where

import Cardano.Node.Emulator.Params (pNetworkId)
import Control.Monad
import Control.Monad.Freer.Extras.Log (LogLevel (Debug, Info))
import Cooked
import Cooked.MockChain.Contract
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Ledger (Address (..), CardanoAddress, CardanoTx, PaymentPubKeyHash (..), PubKeyHash, toPlutusAddress)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
  ( Contract,
    Endpoint,
    Promise,
    endpoint,
    getParams,
    logInfo,
    selectList,
    submitTxConstraints,
    submitTxConstraintsSpending,
    type (.\/), AsContractError, ContractError,
  )
import Plutus.Contract.Test (mockWalletPaymentPubKeyHash, w1, w2)
import qualified Plutus.Script.Utils.Ada as Ada
import Plutus.Script.Utils.Typed (validatorAddress)
import Plutus.Trace (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import Plutus.V1.Ledger.Api
  ( Address,
    Credential (..),
    ScriptContext (ScriptContext, scriptContextTxInfo),
    TxInfo (txInfoOutputs),
    TxOut (TxOut, txOutAddress, txOutValue),
    Value,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    Eq,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    and,
    mapMaybe,
    mconcat,
    ($),
    (&&),
    (-),
    (.),
    (==),
    (>=),
  )
import Prettyprinter (Pretty (pretty), viaShow)
import Test.Tasty
import Wallet.Emulator.Stream (filterLogLevel)
import Wallet.Emulator.Wallet (Wallet, mockWalletAddress)
import Prelude (IO, (<$>), (>>))
import qualified Prelude as Haskell

-- * Onchain for the simple split contract

data SplitData = SplitData
  { -- | First recipient of the funds
    recipient1 :: PubKeyHash,
    -- | Second recipient of the funds
    recipient2 :: PubKeyHash,
    -- | How much Ada we want to lock
    amount :: Ada.Ada
  }
  deriving stock (Haskell.Show, Generic)

instance Eq SplitData where -- this is PlutusTx.Eq
  (SplitData a b c) == (SplitData x y z) = and [a == x, b == y, c == z]

instance Pretty SplitData where
  pretty = viaShow

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit SplitData {recipient1, recipient2, amount} _ ScriptContext {scriptContextTxInfo} =
  let half = Ada.divide amount 2
      outputs = txInfoOutputs scriptContextTxInfo
   in Ada.fromValue (valuePaidToAddr outputs recipient1) >= half
        && Ada.fromValue (valuePaidToAddr outputs recipient2) >= (amount - half)
  where
    valuePaidToAddr :: [TxOut] -> PubKeyHash -> Value
    valuePaidToAddr outs pkh =
      let flt TxOut {txOutAddress = Address (PubKeyCredential txOutPkh) _, txOutValue} | txOutPkh == pkh = Just txOutValue
          flt _ = Nothing
       in mconcat $ mapMaybe flt outs

data Split

instance Scripts.ValidatorTypes Split where
  type RedeemerType Split = ()
  type DatumType Split = SplitData

splitValidator :: Scripts.TypedValidator Split
splitValidator =
  Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [||validateSplit||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @ScriptContext @SplitData @()

-- * Offchain using cooked-validators

lockFunds :: MonadBlockChain m => SplitData -> m ()
lockFunds datum@SplitData {amount} =
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOuts = [paysScript splitValidator datum (Ada.toValue amount)]
        }

unlockFunds :: MonadBlockChain m => SplitData -> m ()
unlockFunds datum@SplitData {recipient1, recipient2, amount} = do
  (oref, _) : _ <-
    filterUtxos
      ( isScriptOutputFrom' splitValidator
          >=> isOutputWithValueSuchThat (== Ada.toValue amount)
          >=> isOutputWithDatumSuchThat (\(ResolvedOrInlineDatum datum') -> datum' == datum)
      )
      <$> (resolveDatums =<< utxosAt (validatorAddress splitValidator))
  let half = Ada.divide amount 2
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript (),
          txSkelOuts =
            [ paysPK recipient1 (Ada.toValue half),
              paysPK recipient2 (Ada.toValue $ amount - half)
            ]
        }

-- * Using the cooked-validators offchain in the 'Contract' monad

data LockArgs = LockArgs
  { recipient1Pkh :: PubKeyHash,
    recipient2Pkh :: PubKeyHash,
    totalAda :: Ada.Ada
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type SplitSchema =
  Endpoint "lock" LockArgs
    .\/ Endpoint "unlock" LockArgs

lock :: (AsMockChainError e, AsContractError e) => Promise () SplitSchema e ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: (AsMockChainError e, AsContractError e) => Promise () SplitSchema e ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs {recipient1Pkh, recipient2Pkh, totalAda} =
  SplitData
    { recipient1 = recipient1Pkh,
      recipient2 = recipient2Pkh,
      amount = totalAda
    }

splitPlutusApp :: Contract () SplitSchema ContractError ()
splitPlutusApp = forever $ selectList [lock, unlock]

splitDataEmulatorTrace :: PubKeyHash -> PubKeyHash -> EmulatorTrace ()
splitDataEmulatorTrace w1Pkh w2Pkh = do
  h <- Trace.activateContractWallet w1 splitPlutusApp
  Trace.callEndpoint @"lock" h $ LockArgs w1Pkh w2Pkh 10_000_000
  void Trace.nextSlot
  Trace.callEndpoint @"unlock" h $ LockArgs w1Pkh w2Pkh 10_000_000

runSplitDataEmulatorTrace :: IO ()
runSplitDataEmulatorTrace =
   Trace.runEmulatorTraceIO $ splitDataEmulatorTrace (walletPKHash $ wallet 1) (walletPKHash $ wallet 2)

tests :: TestTree
tests = testGroup "Contract monad instance" []
