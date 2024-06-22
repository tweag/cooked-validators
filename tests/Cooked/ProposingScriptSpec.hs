module Cooked.PropositingScriptSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Builtins qualified as PlutusTx hiding (head)
import PlutusTx.Eq qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.List qualified as PlutusTx
import PlutusTx.Numeric qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx
import PlutusTx.Trace qualified as PlutusTx

showParameterChangeScript :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
showParameterChangeScript _ ctx =
  let scriptContext = PlutusTx.unsafeFromBuiltinData @Api.ScriptContext ctx
      proposalProcedure = PlutusTx.head $ Api.txInfoProposalProcedures $ Api.scriptContextTxInfo scriptContext
   in case Api.ppGovernanceAction proposalProcedure of
        Api.ParameterChange _ (Api.ChangedParameters dat) _ ->
          let innerMap = PlutusTx.unsafeFromBuiltinData @(PlutusTx.Map PlutusTx.Integer PlutusTx.Integer) dat
           in if innerMap PlutusTx.== PlutusTx.fromList [(0, 100)] then () else PlutusTx.traceError "wrong map"
        _ -> PlutusTx.traceError "Wrong proposal procedure"

showProposingScript :: Script.Versioned Script.Script
showProposingScript =
  mkProposingScript
    $$(PlutusTx.compile [||showParameterChangeScript||])

testProposingScript :: (MonadBlockChain m) => m ()
testProposingScript = do
  (txOutRef, Api.txOutValue -> val) : _ <- runUtxoSearch $ utxosAtSearch $ wallet 1
  deposit <- govActionDeposit
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelIns = Map.singleton txOutRef TxSkelNoRedeemer,
          txSkelOuts = [paysPK (wallet 2) (val <> PlutusTx.negate (toValue deposit <> ada 2))],
          txSkelOpts =
            def
              { txOptBalancingPolicy = DoNotBalance,
                txOptCollateralUtxos = CollateralUtxosFromWallet $ wallet 1,
                txOptFeePolicy = ManualFee 2_000_000
              },
          txSkelProposals =
            [ simpleTxSkelProposal
                (wallet 1)
                (TxGovActionParameterChange [FeePerByte 100])
                `withWitness` (showProposingScript, TxSkelNoRedeemer)
                `withAnchor` "https://lichess.org"
            ]
        }
