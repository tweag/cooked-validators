module Spec.StagedRun where

import Cooked
import Optics.Core
import Plutus.Script.Utils.V3.Generators
import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty (TestTree)

stagedRun :: StagedMockChain ()
stagedRun = do
  -- Defining some aliases for wallets
  alice <- define "alice" $ wallet 1
  bob <- define "bob" $ wallet 2
  carrie <- define "carrie" $ wallet 3
  -- Defining some aliases for scripts
  trueScript <- define "trueScript" $ trueMPScript @()
  falseScript <- define "falseScript" $ falseMPScript @()
  -- Defining some aliases for tokens
  permanent <- define "permanent" $ Api.TokenName "permanent"
  quick <- define "quick" $ Api.TokenName "quick"
  -- Some values
  let permanentValue = Value . review (valueAssetClassAmountP falseScript permanent)
      quickValue = Value . review (valueAssetClassAmountP trueScript quick)
  -- Providing an initial distribution of funds
  outputs <-
    forceOutputs $
      replicate 4 (bob `receives` Value (ada 10))
        ++ replicate 4 (carrie `receives` Value (ada 10))
        ++ replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` permanentValue 3 <&&> InlineDatum (3 :: Integer),
             alice `receives` permanentValue 5 <&&> HiddenHashedDatum (15 :: Integer),
             alice `receives` quickValue 4,
             alice `receives` quickValue 10 <&&> VisibleHashedDatum (25 :: Integer),
             alice `receives` permanentValue 12 <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` InlineDatum (20 :: Integer)
           ]
  -- Ensuring that "Alice" got 10 utxos out of the "forceOutputs" call
  aliceUtxos <-
    beginSearch (return outputs)
      & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
  assert' $ length aliceUtxos == 10
  -- Ensuring that Alice has 2 utxos with quick values with the right amount
  aliceQuickValueExtracts <-
    getExtracts $
      beginSearch (return outputs)
        & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP trueScript quick)
  assert' $ aliceQuickValueExtracts == ((`HCons` HEmpty) <$> [4, 10])
  -- Ensuring the Alice has 2 utxos created with hashed datums with permanent
  -- values, and retrieving the typed content of those datums.
  aliceHashedDatums <-
    getExtracts $
      beginSearch (return outputs)
        & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP falseScript permanent)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumKindAT % datumKindResolvedP)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumTypedAT @Integer)
  assert' $
    aliceHashedDatums
      == [ HCons 5 (HCons NotResolved (HCons 15 HEmpty)),
           HCons 12 (HCons Resolved (HCons 10 HEmpty))
         ]
  return ()

tests :: TestTree
tests = testCooked "Full staged run" $ mustSucceedTest stagedRun `withInitDist` InitialDistribution []
