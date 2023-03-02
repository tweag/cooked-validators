{-# LANGUAGE NumericUnderscores #-}

-- | Standard transactions that ought to cover a substantial spectrum of the
-- capabilities of Cooked.
module Cooked.Behaviour.Elementary where

import Cooked
import qualified Cooked.Behaviour.Validators as Validators
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

-- | Get all outputs locked by a validator from a blockchain.
outputsProtectedBy ::
  MonadBlockChainBalancing m =>
  Scripts.TypedValidator Validators.Unit ->
  m [(PV2.TxOutRef, PV2.TxOut)]
outputsProtectedBy =
  runUtxoSearch
    . utxosAtSearch
    . Address.mkValidatorAddress
    . Scripts.validatorScript

walletToWallet :: Assertion
walletToWallet =
  testSucceedsFrom'
    def
    ( \_ state -> do
        holdingInState state (wallet 2) @?= Pl.adaValueOf 7
        countLovelace (holdingInState state (wallet 1))
          <= 3_000_000
          @? "Wallet 1 has too many Lovelace"
    )
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 5]),
            (wallet 2, [Pl.adaValueOf 5])
          ]
    )
    ( validateTxSkel $
        txSkelTemplate
          { txSkelOuts = [paysPK (walletPKHash $ wallet 2) (Pl.adaValueOf 2)],
            txSkelSigners = [wallet 1]
          }
    )

walletToScript :: Assertion
walletToScript =
  testSucceedsFrom'
    def
    ( \yesOuts state -> do
        let yesOutsValue = mconcat $ view outputValueL . snd <$> yesOuts
        countLovelace yesOutsValue @?= 2_000_000
        -- Wallet 1 pays some fees
        countLovelace (holdingInState state (wallet 1))
          < 3_000_000
          @? "Wallet 1 has too many Lovelace (have fees been spent?)"
        case yesOuts of [_] -> True; _ -> False
          @? "There is not a single output locked by the yes validator"
    )
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 5]),
            (wallet 2, [Pl.adaValueOf 5])
          ]
    )
    ( let skel =
            txSkelTemplate
              { txSkelOuts = [paysScript Validators.yes () (Pl.adaValueOf 2)],
                txSkelSigners = [wallet 1]
              }
       in validateTxSkel skel >> outputsProtectedBy Validators.yes
    )

-- | Create transaction with an output protected by a script.
walletToScriptHowDatum ::
  -- | How to store the datum on the output
  Validators.DatumKind ->
  Assertion
walletToScriptHowDatum datumKind =
  testSucceedsFrom'
    def
    ( \outputs state -> do
        let outsValue = mconcat $ view outputValueL . snd <$> outputs
        countLovelace outsValue @?= 2_000_000
        -- Wallet 1 pays some fees
        countLovelace (holdingInState state (wallet 1))
          < 3_000_000
          @? "Wallet 1 has too many Lovelace (have fees been spent?)"
        ( case ( datumKind,
                 mapMaybe
                   (fmap outputOutputDatum . (isScriptOutputFrom Validators.yes . snd))
                   outputs
               ) of
            (Validators.Inline, [PV2.OutputDatum _]) -> True
            (Validators.OnlyHash, [PV2.OutputDatumHash _]) -> True
            (Validators.ResolvedHash, [PV2.OutputDatumHash _]) -> True
            _ -> False
          )
          @? "The datum kind is not the one expected"
    )
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 5]),
            (wallet 2, [Pl.adaValueOf 5])
          ]
    )
    ( let pays ::
            Scripts.TypedValidator Validators.Unit ->
            Scripts.DatumType Validators.Unit ->
            PV2.Value ->
            TxSkelOut
          pays = case datumKind of
            Validators.Inline -> paysScriptInlineDatum
            Validators.OnlyHash -> paysScriptDatumHash
            Validators.ResolvedHash -> paysScript
          skel =
            txSkelTemplate
              { txSkelOuts = [pays Validators.yes () (Pl.adaValueOf 2)],
                txSkelSigners = [wallet 1]
              }
       in validateTxSkel skel >> outputsProtectedBy Validators.yes
    )

tests :: TestTree
tests =
  testGroup
    "Transferring Ada from a wallet to"
    [ testCase "another wallet" walletToWallet,
      testCase "a script" walletToScript,
      testCase
        "a script, using an inline datum"
        (walletToScriptHowDatum Validators.Inline),
      testCase
        "a script, using a hashed datum"
        (walletToScriptHowDatum Validators.OnlyHash),
      testCase
        "a script, using a resolved datum"
        $ walletToScriptHowDatum Validators.ResolvedHash
    ]
