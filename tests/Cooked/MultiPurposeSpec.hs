module Cooked.MultiPurposeSpec where

import Cooked.ShowBS
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

data MintingRed = Mint Api.TxOutRef | Burn

txOutRefToTokenName :: Api.TxOutRef -> Api.TokenName
txOutRefToTokenName (Api.TxOutRef (Api.TxId txId) ix) = Api.TokenName txId

{-# INLINEABLE mpMintingPurpose #-}
mpMintingPurpose :: Script.MintingScriptType MintingRed Api.TxInfo
mpMintingPurpose cs@(Api.CurrencySymbol hash) (Mint oRef) (Api.TxInfo {..}) =
  let requiredMintedValue = Api.Value (Map.singleton cs $ Map.singleton (txOutRefToTokenName oRef) 1)
   in Script.toValue txInfoMint == requiredMintedValue
        && case filter (\out -> Api.addressCredential (Api.txOutAddress out) == Api.ScriptCredential (Api.ScriptHash hash)) txInfoOutputs of
          [o] -> Script.currencyValueOf (Api.txOutValue o) cs == requiredMintedValue
          _ -> False
mpMintingPurpose (Api.CurrencySymbol hash) Burn (Api.TxInfo {..}) = undefined

tests :: TestTree
tests = testGroup "Multi purpose scripts" []
