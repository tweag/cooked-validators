{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.StagedRun where

import Control.Monad
import Cooked
import Data.Default
import Optics.Core
import Plutus.Script.Utils.V3.Generators
import Plutus.Script.Utils.V3.Typed
import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3 qualified as Api
import Polysemy.Bundle
import Polysemy.Reader
import Polysemy.State
import Test.Tasty (TestTree)

{--
Script for the demo :

Enter a repl and load the required dependencies.

> cabal repl tests
> :l tests/Spec/StagedRun
> import Cooked

- We define a run which does nothing
--}

demoRun :: StagedMockChain ()
demoRun = return ()

{--
Maybe we want to include payments directly within the run (as opposed to outside of the run)
--}

demoRunForceOutputs :: StagedMockChain ()
demoRunForceOutputs =
  forceOutputs_ $
    replicate 4 (wallet 2 `receives` Value (ada 10))
      ++ replicate 4 (wallet 1 `receives` Value (ada 10))
      ++ [wallet 1 `receives` InlineDatum (20 :: Integer)]

{--
Notice that cooked already made some adjustments to the value of an input (min ADA accounted for).
We can actually ask cooked to show the adjustement it mades during the run.
--}

printAndRunWithLog :: (Show a) => StagedMockChain a -> IO ()
printAndRunWithLog = printCookedOpt (def {pcOptPrintLog = True}) . runMockChainDef

{--
However, we could also tell cooked not to do any adjustment, which would result in a
UTxO which could not exist on-chain.
--}

demoRunForceOutputsNoMinAda :: StagedMockChain ()
demoRunForceOutputsNoMinAda =
  forceOutputs_ $
    replicate 4 (wallet 2 `receives` Value (ada 10))
      ++ replicate 4 (wallet 1 `receives` Value (ada 10))
      ++ [wallet 1 `receives` InlineDatum (20 :: Integer) <&&> FixedValue (ada 0)]

{--
In practice, there is no insight to do that in such cases, but during audits it can often be useful to
check that the computations were done right in the offchain code.

Let's now move on to some more complex and interesting initial values.
--}

demoRunFullForcedOutputs :: StagedMockChain ()
demoRunFullForcedOutputs = do
  let alice = wallet 1
      bob = wallet 2
      trueScript = trueMPScript @()
      falseScript = falseMPScript @()
      permanent = Api.TokenName "permanent"
      quick = Api.TokenName "quick"
      permanentValue = review (valueAssetClassAmountP falseScript permanent)
      quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (bob `receives` Value (ada 10))
      ++ replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (permanentValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (permanentValue 5) <&&> HiddenHashedDatum (15 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (25 :: Integer),
           alice `receives` Value (permanentValue 12) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` InlineDatum (20 :: Integer)
         ]

{--
Notice that cooked made min ADA adjustment to all of the relevant inputs. We can also
notice that some script have aliases, and the wallets are labelled wallet 1 to wallet 10
but some other entities such as the token names are represented by their hashes.
Thankfully, we can freely create nicknames for everything when initializing variables.
--}

demoRunNicknames :: StagedMockChain ()
demoRunNicknames = do
  alice <- define "alice" $ wallet 1
  bob <- define "bob" $ wallet 2
  trueScript <- define "trueScript" $ trueMPScript @()
  falseScript <- define "falseScript" $ falseMPScript @()
  permanent <- define "permanent" $ Api.TokenName "permanent"
  quick <- define "quick" $ Api.TokenName "quick"
  let permanentValue = review (valueAssetClassAmountP falseScript permanent)
      quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (bob `receives` Value (ada 10))
      ++ replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (permanentValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (permanentValue 5) <&&> HiddenHashedDatum (15 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (25 :: Integer),
           alice `receives` Value (permanentValue 12) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` InlineDatum (20 :: Integer)
         ]

{--
While the aliasing is perfect, it relies on static data for now, and data manually written.
There are many cases where this should rely on dynamic data / data coming from the client
codebase. Thankfully, a StagedMockChain can be extended to integrate arbritrary effects, such
as a State or a Reader effect, within a bundle (if several effects are used) placed at the
right location within the effect stack. Let's rewrite the above using this capability.
--}

data Environment = Environment
  { alice :: Wallet,
    bob :: Wallet,
    trueScript :: MultiPurposeScript (),
    falseScript :: MultiPurposeScript (),
    permanent :: Api.TokenName,
    quick :: Api.TokenName
  }

initialEnvironment :: Environment
initialEnvironment =
  Environment
    (wallet 1)
    (wallet 2)
    trueMPScript
    falseMPScript
    (Api.TokenName "permanent")
    (Api.TokenName "quick")

demoRunNicknamesEnv :: ExtendedStagedMockChain (Reader Environment) ()
demoRunNicknamesEnv = do
  alice <- asks alice >>= define "alice"
  bob <- asks bob >>= define "bob"
  trueScript <- asks trueScript >>= define "trueScript"
  falseScript <- asks falseScript >>= define "falseScript"
  permanent <- asks permanent >>= define "permanent"
  quick <- asks quick >>= define "quick"
  let permanentValue = review (valueAssetClassAmountP falseScript permanent)
      quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (bob `receives` Value (ada 10))
      ++ replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (permanentValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (permanentValue 5) <&&> HiddenHashedDatum (15 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (25 :: Integer),
           alice `receives` Value (permanentValue 12) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` InlineDatum (20 :: Integer)
         ]

{--
Let's try to run this as we did before (we notice it does not work because we lack an instance)
--}

instance InterpretAlone (Reader Environment) where
  runInterpretAlone = runReader initialEnvironment

-- >
-- Script for the demo :
--
-- - Run an empty "StagedMockChain": notice that we see a non-empty state (Initial distribution)
--
-- - Run it without the initial distribution ! Clean slate !
--
-- - Add a "forceOutputs" call with assets given to peers, notice a similar ending state
--
-- - Notice that the ending state is unreadable, add aliases
--
-- - Make some queries about existing utxos (show the resulting types, the filtering)
--
-- - Make some tracing about the results, and some assertions as well (they only take effect in testing mode)

instance InterpretAlone (Bundle '[State Integer]) where
  runInterpretAlone = evalState 0 . runBundle

stagedRun :: ExtendedStagedMockChain (Bundle '[State Integer]) Integer
stagedRun = do
  -- Defining some aliases for wallets
  alice <- define "alice" $ wallet 1
  bob <- define "bob" $ wallet 2
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
        ++ replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` permanentValue 3 <&&> InlineDatum (3 :: Integer),
             alice `receives` permanentValue 5 <&&> HiddenHashedDatum (15 :: Integer),
             alice `receives` quickValue 4,
             alice `receives` quickValue 10 <&&> VisibleHashedDatum (25 :: Integer),
             alice `receives` permanentValue 12 <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` InlineDatum (20 :: Integer)
           ]
  noteS "We have given a few assets to Alice, Bob and Carry to begin the run"
  -- Ensuring that "Alice" got 10 utxos out of the "forceOutputs" call
  aliceUtxos <-
    beginSearchP outputs
      & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
  assert' $ length aliceUtxos == 10
  forM_ (zip [(1 :: Integer) ..] aliceUtxos) $ \(i, (_, output)) -> noteL ("Alice UTxO number " <> show i) output
  -- Ensuring that Alice has 2 utxos with quick values with the right amount
  aliceQuickValueExtracts <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP trueScript quick)
  assert' $ aliceQuickValueExtracts == ((`HCons` HEmpty) <$> [4, 10])
  -- Ensuring that Alice has 2 utxos created with hashed datums with permanent
  -- values, and retrieving the typed content of those datums.
  aliceHashedDatums <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userEitherPubKeyP % userTypedPubKeyAT @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP falseScript permanent)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumKindAT % datumKindResolvedP)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumTypedAT @Integer)
  assert' $
    aliceHashedDatums
      == [ HCons 5 (HCons NotResolved (HCons 15 HEmpty)),
           HCons 12 (HCons Resolved (HCons 10 HEmpty))
         ]
  mplus (sendBundle $ put 10) (sendBundle $ put 20)
  sendBundle get

tests :: TestTree
tests = testCooked "Full staged run" $ mustSucceedTest stagedRun
