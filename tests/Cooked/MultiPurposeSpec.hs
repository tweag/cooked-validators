{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules defines a small dummy smart contract which can both be used as
-- a spending or minting script. The smart contract is parameterized with a
-- transaction. It allows the minting of as manay NFTs as there are outputs to
-- this transactions. Each NFT can be minted alone, in a transaction that
-- consumes one of these outputs. They must be put at the script minting script
-- address to allow for the spending purpose to be used, with a datum containing
-- an integer. This integer must be equal to the index at which the TxOutRef was
-- produced in the parameter transaction. Then, if the datum is 0, the token can
-- be burned while consuming its UTXO. If is it greater than 0, 1 by 1 steps can
-- descrease the counter until it reaches 0, at which point it can be burned.
module Cooked.MultiPurposeSpec where

import Cooked
import Data.Map qualified as HMap
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.Internal qualified as PlutusTx
import PlutusTx.Prelude
import Prettyprinter qualified as PP
import Test.Tasty
import Test.Tasty.HUnit
import Prelude qualified as HS

instance Eq Api.ScriptPurpose where
  Api.Spending r1 == Api.Spending r2 = r1 == r2
  Api.Minting r1 == Api.Minting r2 = r1 == r2
  _ == _ = False

data MintingRed = Mint Api.TxOutRef | Burn
  deriving (HS.Show, HS.Eq)

instance PrettyCooked MintingRed where
  prettyCooked = PP.viaShow

instance Eq MintingRed where
  (==) = (HS.==)

PlutusTx.unstableMakeIsData ''MintingRed
PlutusTx.makeLift ''MintingRed

data SpendingRed = Step | Close
  deriving (HS.Show, HS.Eq)

instance PrettyCooked SpendingRed where
  prettyCooked = PP.viaShow

instance Eq SpendingRed where
  (==) = (HS.==)

PlutusTx.unstableMakeIsData ''SpendingRed
PlutusTx.makeLift ''SpendingRed

{-# INLINEABLE txOutRefToToken #-}
txOutRefToToken :: Api.TxOutRef -> Api.TokenName
txOutRefToToken (Api.TxOutRef (Api.TxId txId) n) = Api.TokenName $ sha2_256 (txId <> encodeInteger n)

{-# INLINEABLE txOutRefToToken' #-}
txOutRefToToken' :: Integer -> Api.TxOutRef -> Api.TokenName
txOutRefToToken' ix (Api.TxOutRef (Api.TxId txId) n) =
  Api.TokenName . consByteString ix . sliceByteString 1 31 $ sha2_256 (txId <> encodeInteger n)

{-# INLINEABLE encodeInteger #-}
encodeInteger :: Integer -> PlutusTx.BuiltinByteString
encodeInteger x
  | x < 256 = PlutusTx.consByteString x ""
  | otherwise = encodeInteger (x `quotient` 256) <> consByteString (x `modulo` 256) ""

{-# INLINEABLE mpMintingPurpose #-}
mpMintingPurpose :: Api.TxId -> Script.MintingScriptType MintingRed Api.TxInfo
mpMintingPurpose txId cs@(Api.CurrencySymbol hash) (Mint oRef@(Api.TxOutRef txId' ix)) (Api.TxInfo {..}) =
  let requiredMintedValue = Api.assetClassValue (Api.assetClass cs (txOutRefToToken oRef)) 1
   in Script.toValue txInfoMint
        == requiredMintedValue
        && length
          [ val
            | Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash hash')) _) val (Api.OutputDatum (Api.Datum dat)) _ <- txInfoOutputs,
              hash == hash',
              Script.currencyValueOf val cs == requiredMintedValue,
              Api.fromBuiltinData dat == Just ix
          ]
        == 1
        && oRef
        `elem` (Api.txInInfoOutRef <$> txInfoInputs)
        && txId
        == txId'
mpMintingPurpose _ cs@(Api.CurrencySymbol hash) Burn (Api.TxInfo {..}) =
  Api.Value
    ( Map.singleton cs
        $ Map.safeFromList
          [ (tn, n)
            | Api.TxInInfo scriptRef (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash hash')) _) val (Api.OutputDatum (Api.Datum dat)) _) <- txInfoInputs,
              hash' == hash,
              Api.fromBuiltinData @Integer dat == Just 0,
              (cs', tn, n) <- Api.flattenValue val,
              cs == cs',
              Map.lookup (Api.Spending scriptRef) txInfoRedeemers == Just (Api.Redeemer (Api.toBuiltinData Close))
          ]
    )
    == negate (Script.toValue txInfoMint)

{-# INLINEABLE mpSpendingPurpose #-}
mpSpendingPurpose :: Script.SpendingScriptType Integer SpendingRed Api.TxInfo
mpSpendingPurpose oRef (Just x) Close Api.TxInfo {..}
  | x == 0 =
      length
        [ h
          | Api.TxInInfo oRef' (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfoInputs,
            oRef == oRef',
            Map.lookup (Api.Minting (Api.CurrencySymbol h)) txInfoRedeemers == Just (Api.Redeemer (Api.toBuiltinData Burn))
        ]
        == 1
mpSpendingPurpose oRef (Just x) Step Api.TxInfo {..} =
  length
    [ h
      | Api.TxInInfo oRef' (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) val _ _) <- txInfoInputs,
        oRef == oRef',
        Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h')) _) val' (Api.OutputDatum (Api.Datum dat')) _ <- txInfoOutputs,
        h' == h,
        Script.noAdaValue val == Script.noAdaValue val',
        Api.fromBuiltinData dat' == Just (x - 1)
    ]
    == 1
mpSpendingPurpose _ _ _ _ = False

instance Script.MultiPurposeScriptTypes () where
  type SpendingRedeemerType () = SpendingRed
  type SpendingDatumType () = Integer
  type MintingRedeemerType () = MintingRed

mpScript :: Api.TxId -> Script.MultiPurposeScript ()
mpScript txId = Script.MultiPurposeScript $ Script.toScript $ $$(PlutusTx.compile [||script||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef txId
  where
    script txId' =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withSpendingPurpose` mpSpendingPurpose
        `Script.withMintingPurpose` mpMintingPurpose txId'

alice, bob :: Wallet
alice = wallet 1
bob = wallet 2

runScript :: (MonadModalBlockChain m) => m ()
runScript = do
  [oRef@(Api.TxOutRef txId _), oRef', oRef''] <-
    validateTxSkel'
      $ txSkelTemplate
        { txSkelOuts =
            [ alice `receives` Value (Script.ada 3),
              alice `receives` Value (Script.ada 5)
            ],
          txSkelSigners = [bob]
        }

  let script = mpScript txId
      (mintSkel1, _, tn1) = mkMintSkel alice oRef script
      (mintSkel2, mintValue2, tn2) = mkMintSkel alice oRef' script
      (mintSkel3, mintValue3, tn3) = mkMintSkel bob oRef'' script

  (oRefScript : _) <- validateTxSkel' mintSkel1
  (oRefScript1 : _) <- validateTxSkel' mintSkel2
  (oRefScript2 : _) <- validateTxSkel' mintSkel3

  (oRefScript1' : oRefScript2' : _) <-
    validateTxSkel'
      $ txSkelTemplate
        { txSkelSigners = [alice],
          txSkelIns =
            HMap.fromList
              [ (oRefScript, someTxSkelRedeemer Close),
                (oRefScript1, someTxSkelRedeemer Step),
                (oRefScript2, someTxSkelRedeemer Step)
              ],
          txSkelOuts =
            [ script `receives` (InlineDatum (0 :: Integer) <&&> Value mintValue2),
              script `receives` (InlineDatum (1 :: Integer) <&&> Value mintValue3)
            ],
          txSkelMints = txSkelMintsFromList [(Script.toVersioned @Script.MintingPolicy script, someTxSkelRedeemer Burn, tn1, -1)]
        }

  (oRefScript2'' : _) <-
    validateTxSkel'
      $ txSkelTemplate
        { txSkelSigners = [bob],
          txSkelIns =
            HMap.fromList
              [ (oRefScript1', someTxSkelRedeemer Close),
                (oRefScript2', someTxSkelRedeemer Step)
              ],
          txSkelOuts =
            [ script `receives` (InlineDatum (0 :: Integer) <&&> Value mintValue3)
            ],
          txSkelMints = txSkelMintsFromList [(Script.toVersioned @Script.MintingPolicy script, someTxSkelRedeemer Burn, tn2, -1)]
        }

  validateTxSkel_
    $ txSkelTemplate
      { txSkelSigners = [alice],
        txSkelIns = HMap.singleton oRefScript2'' (someTxSkelRedeemer Close),
        txSkelMints = txSkelMintsFromList [(Script.toVersioned @Script.MintingPolicy script, someTxSkelRedeemer Burn, tn3, -1)]
      }
  where
    mkMintSkel :: Wallet -> Api.TxOutRef -> Script.MultiPurposeScript () -> (TxSkel, Api.Value, Api.TokenName)
    mkMintSkel signer oRef@(Api.TxOutRef _ ix) script =
      let tn = txOutRefToToken oRef
          mints = txSkelMintsFromList' [(Script.toVersioned @Script.MintingPolicy script, someTxSkelRedeemer (Mint oRef), [(tn, 1)])]
          mintValue = txSkelMintsValue mints
       in ( txSkelTemplate
              { txSkelIns = HMap.singleton oRef emptyTxSkelRedeemer,
                txSkelMints = mints,
                txSkelOuts = [script `receives` (InlineDatum ix <&&> Value (txSkelMintsValue mints))],
                txSkelSigners = [signer]
              },
            mintValue,
            tn
          )

tests :: TestTree
tests =
  testGroup
    "Multi purpose scripts"
    [ testCase "Using a script as minting and spending in the same scenario" $ testSucceeds runScript,
      testGroup
        "The Spending purpose behaves properly"
        [ testCase "We cannot redirect any output to a private key"
            $ testToProp
            $ mustFailTest
              ( somewhere
                  (datumHijackingAttack @(Script.MultiPurposeScript ()) alice)
                  runScript
              )
            `withExactSize` 6,
          testCase "We cannot redirect any output to another script"
            $ testToProp
            $ mustFailTest
              ( somewhere
                  (datumHijackingAttack @(Script.MultiPurposeScript ()) (Script.trueSpendingMPScript @()))
                  runScript
              )
            `withExactSize` 6
        ],
      testGroup
        "The Minting purpose behaves properly"
        [ testCase "We cannot duplicate the tokens"
            $ testToProp
            $ mustFailTest (somewhere (dupTokenAttack (\_ n -> n + 1) alice) runScript)
            `withExactSize` 6,
          testCase
            "We cannot mint additional tokens"
            $ testToProp
            $ mustFailTest (somewhere (addTokenAttack (const [(Api.TokenName "myToken", 1)]) alice) runScript)
            `withExactSize` 6
        ]
    ]
