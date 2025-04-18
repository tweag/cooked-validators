{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules defines a small dummy smart contract which can both be used as
-- a spending or minting script. The smart contract is parameterized with a
-- transaction id. It allows the minting of as many NFTs as there are outputs to
-- this transactions. Each NFT has to be minted alone, in a transaction that
-- consumes exactly one of these outputs. They must be put at the script address
-- to allow for the spending purpose to be used, with a datum containing an
-- integer. This integer must be equal to the index at which the TxOutRef was
-- produced in the parameter transaction. Then, if the datum is 0, the token can
-- be burned while consuming its UTXO. If is it greater than 0, 1 by 1 steps can
-- decrease the counter until it reaches 0, at which point it can be burned.
module Cooked.MultiPurposeSpec where

import Cooked
import Data.Map qualified as HMap
import Plutus.Script.Utils.V3 qualified as Script
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

data MintingRed = MintToken Api.TxOutRef | BurnToken
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

{-# INLINEABLE encodeInteger #-}
encodeInteger :: Integer -> Api.BuiltinByteString
encodeInteger x
  | x < 256 = PlutusTx.consByteString x ""
  | otherwise = consByteString (x `modulo` 256) $ encodeInteger (x `quotient` 256)

data MPTag

instance Script.MultiPurposeScriptTypes MPTag where
  type SpendingRedeemerType MPTag = SpendingRed
  type SpendingDatumType MPTag = Integer
  type MintingRedeemerType MPTag = MintingRed

{-# INLINEABLE mpMintingPurpose #-}
mpMintingPurpose :: Api.TxId -> Script.MintingPurposeType MPTag
mpMintingPurpose txId cs@(Api.CurrencySymbol hash) (MintToken oRef@(Api.TxOutRef txId' ix)) (Api.TxInfo {..}) =
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
mpMintingPurpose _ cs@(Api.CurrencySymbol hash) BurnToken (Api.TxInfo {..}) =
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
mpSpendingPurpose :: Script.SpendingPurposeType MPTag
mpSpendingPurpose oRef (Just x) Close Api.TxInfo {..}
  | x == 0 =
      length
        [ h
          | Api.TxInInfo oRef' (Api.TxOut (Api.Address (Api.ScriptCredential (Api.ScriptHash h)) _) _ _ _) <- txInfoInputs,
            oRef == oRef',
            Map.lookup (Api.Minting (Api.CurrencySymbol h)) txInfoRedeemers == Just (Api.Redeemer (Api.toBuiltinData BurnToken))
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

mpScript :: Api.TxId -> Script.MultiPurposeScript MPTag
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

  script <- define "My multipurpose script" $ mpScript txId
  let (mintSkel1, _, tn1) = mkMintSkel alice oRef script
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
          txSkelMints = txSkelMintsFromList [burn script (someTxSkelRedeemer BurnToken) tn1 1]
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
          txSkelMints = txSkelMintsFromList [burn script (someTxSkelRedeemer BurnToken) tn2 1]
        }

  validateTxSkel_
    $ txSkelTemplate
      { txSkelSigners = [alice],
        txSkelIns = HMap.singleton oRefScript2'' (someTxSkelRedeemer Close),
        txSkelMints = txSkelMintsFromList [burn script (someTxSkelRedeemer BurnToken) tn3 1]
      }
  where
    mkMintSkel :: Wallet -> Api.TxOutRef -> Script.MultiPurposeScript MPTag -> (TxSkel, Api.Value, Api.TokenName)
    mkMintSkel signer oRef@(Api.TxOutRef _ ix) script =
      let tn = txOutRefToToken oRef
          mints = txSkelMintsFromList [mint script (someTxSkelRedeemer (MintToken oRef)) tn 1]
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
                  (datumHijackingAttack @(Script.MultiPurposeScript MPTag) alice)
                  runScript
              )
            `withExactSize` 6,
          testCase "We cannot redirect any output to another script"
            $ testToProp
            $ mustFailTest
              ( somewhere
                  (datumHijackingAttack @(Script.MultiPurposeScript MPTag) (Script.trueSpendingMPScript @()))
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
