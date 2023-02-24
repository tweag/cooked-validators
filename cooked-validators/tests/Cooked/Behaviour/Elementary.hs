{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Standard transactions that ought to cover a substantial spectrum of the
-- capabilities of Cooked.
module Cooked.Behaviour.Elementary where

import Cooked
import qualified Cooked.Behaviour.Validators as Validators
import qualified Data.Map.Strict as Map
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.V2.Address as Address
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Tasty
import Test.Tasty.HUnit

-- | Return the number of Lovelace of the value.
countLovelace :: PV2.Value -> Integer
countLovelace v = Pl.valueOf v PV2.adaSymbol PV2.adaToken

-- | Get all outputs locked by the validator 'Validators.yes' from a
-- blockchain.
yesOutputsOfChain :: MonadBlockChainBalancing m => m [(PV2.TxOutRef, PV2.TxOut)]
yesOutputsOfChain =
  runUtxoSearch . utxosAtSearch $
    Address.mkValidatorAddress
      (Scripts.validatorScript Validators.yes)

walletToWallet :: Assertion
walletToWallet =
  let dist =
        InitialDistribution $
          Map.fromList
            [ (wallet 1, [Pl.adaValueOf 5]),
              (wallet 2, [Pl.adaValueOf 5])
            ]
   in case runMockChainFrom dist . validateTxSkel $
        txSkelTemplate
          { txSkelOuts = [paysPK (walletPKHash $ wallet 2) (Pl.adaValueOf 2)],
            txSkelSigners = [wallet 1]
          } of
        Left err -> assertFailure (renderString prettyCooked err)
        Right (_, state) -> do
          holdingInState state (wallet 2) @?= Pl.adaValueOf 7
          countLovelace (holdingInState state (wallet 1))
            <= 3_000_000
            @? "Wallet 1 has too many Lovelace"

walletToScript :: Assertion
walletToScript =
  let dist =
        InitialDistribution $
          Map.fromList
            [ (wallet 1, [Pl.adaValueOf 5]),
              (wallet 2, [Pl.adaValueOf 5])
            ]
      skel =
        txSkelTemplate
          { txSkelOuts = [paysScript Validators.yes () (Pl.adaValueOf 2)],
            txSkelSigners = [wallet 1]
          }
   in case runMockChainFrom dist $ validateTxSkel skel >> yesOutputsOfChain of
        Left err -> assertFailure (renderString prettyCooked err)
        Right (yesOuts, state) -> do
          let yesOutsValue = mconcat $ view outputValueL . snd <$> yesOuts
          countLovelace yesOutsValue @?= 2_000_000
          -- Wallet 1 pays some fees
          countLovelace (holdingInState state (wallet 1))
            < 3_000_000
            @? "Wallet 1 has too many Lovelace (have fees been spent?)"
          case yesOuts of [_] -> True; _ -> False
            @? "There is not a single output locked by the yes validator"
          -- TODO Ensure the datum is inline

walletToScriptInlineDatum :: Assertion
walletToScriptInlineDatum =
  let dist =
        InitialDistribution $
          Map.fromList
            [ (wallet 1, [Pl.adaValueOf 5]),
              (wallet 2, [Pl.adaValueOf 5])
            ]
      skel =
        txSkelTemplate
          { txSkelOuts = [paysScriptInlineDatum Validators.yes () (Pl.adaValueOf 2)],
            txSkelSigners = [wallet 1]
          }
   in case runMockChainFrom dist $ validateTxSkel skel >> yesOutputsOfChain of
        Left err -> assertFailure (renderString prettyCooked err)
        Right (yesOuts, state) -> do
          let yesOutsValue = mconcat $ view outputValueL . snd <$> yesOuts
          countLovelace yesOutsValue @?= 2_000_000
          -- Wallet 1 pays some fees
          countLovelace (holdingInState state (wallet 1))
            < 3_000_000
            @? "Wallet 1 has too many Lovelace (have fees been spent?)"

tests :: TestTree
tests =
  testGroup
    "Elementary transactions"
    [ testCase "Transferring Ada between wallets" walletToWallet,
      testCase "Transferring Ada to a script" walletToScript,
      testCase
        "Transferring Ada to a script with an inline datum"
        walletToScriptInlineDatum
    ]
