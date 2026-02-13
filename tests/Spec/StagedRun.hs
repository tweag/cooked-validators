{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.StagedRun where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.V3.Generators
import Plutus.Script.Utils.V3.Typed
import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Polysemy
import Polysemy.Reader
import Test.Tasty (TestTree)

printAndRun ::
  ( Show a,
    RunnableMockChain effs,
    Polysemy.Member MockChainWrite effs
  ) =>
  Sem effs a ->
  IO ()
printAndRun =
  printCookedOpt
    ( def
        { pcOptPrintLog = True,
          pcOptHashes = def, -- {pcOptHashNames = mempty},
          pcOptPrintConsumedUTxOs = True,
          pcOptPrintRemainingUTxOs = True
        }
    )
    . runMockChainDef

{--
Script for the demo :

Enter a repl and load the required dependencies.

> cabal repl tests
> :l tests/Spec/StagedRun
> import Cooked

We define a run which does nothing
--}

demoRun :: StagedMockChain ()
demoRun = return ()

{--
We run it using "printAndRun".
It's not too impressive, as there's nothing there.
Let's add some initial assets.
--}

demoRunForceOutputs :: StagedMockChain ()
demoRunForceOutputs =
  forceOutputs_
    [ wallet 2 `receives` Value (ada 10),
      wallet 1 `receives` ReferenceScript (trueMPScript @()),
      wallet 1 `receives` InlineDatum (20 :: Integer),
      wallet 2 `receives` VisibleHashedDatum (10 :: Integer)
    ]

{--
WE RUN IT

Notice that there are hashes everywhere. Let's make some aliases
to improve readability of the mockchain state. We will do two things:
\* add aliases
\* change the running option to have the default hashmap
--}

demoRunForceOutputsAliases :: StagedMockChain ()
demoRunForceOutputsAliases = do
  alice <- define "alice" $ wallet 1
  bob <- define "bob" $ wallet 2
  trueScript <- define "trueScript" $ trueMPScript @()
  forceOutputs_
    [ bob `receives` Value (ada 10),
      alice `receives` ReferenceScript trueScript,
      alice `receives` InlineDatum (20 :: Integer),
      bob `receives` VisibleHashedDatum (10 :: Integer)
    ]

{--
Notice that cooked already made some adjustments to the value of an input (min ADA accounted for).
We can actually ask cooked to show the adjustement it mades during the run.
(we change the opt in the runner above)

WE RUN IT

However, we could also tell cooked not to do any adjustment, which would result in a
UTxO which could not exist on-chain.

WE SKIP IT

In practice, there is no insight to do that in such cases, but during audits it can often be useful to
check that the computations were done right in the offchain code.

Let's now move on to some more complex and interesting initial values.
--}

demoRunFullForcedOutputs :: StagedMockChain ()
demoRunFullForcedOutputs = do
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

instance InterpretAlone (Reader Environment) where
  runInterpretAlone = runReader initialEnvironment

{--
RUN IT

Now, let's make some assertions, searches and traces
--}

demoRunSearches :: ExtendedStagedMockChain (Reader Environment) ()
demoRunSearches = do
  alice <- asks alice >>= define "alice"
  bob <- asks bob >>= define "bob"
  trueScript <- asks trueScript >>= define "trueScript"
  falseScript <- asks falseScript >>= define "falseScript"
  permanent <- asks permanent >>= define "permanent"
  quick <- asks quick >>= define "quick"
  let permanentValue = review (valueAssetClassAmountP falseScript permanent)
      quickValue = review (valueAssetClassAmountP trueScript quick)
  outputs <-
    forceOutputs $
      replicate 4 (bob `receives` Value (ada 10))
        ++ replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` Value (permanentValue 3) <&&> InlineDatum (3 :: Integer),
             alice `receives` Value (permanentValue 5) <&&> HiddenHashedDatum (15 :: Integer),
             alice `receives` Value (quickValue 4),
             alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (25 :: Integer),
             alice `receives` Value (permanentValue 12) <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` InlineDatum (20 :: Integer)
           ]
  noteS "We have given a few assets to Alice, Bob and Carry to begin the run"
  -- Ensuring that "Alice" got 10 utxos out of the "forceOutputs" call
  aliceUtxos <-
    beginSearchP outputs
      & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
  assert "Alice has the right amount of utxos" $ length aliceUtxos == 10
  forM_ (zip [(1 :: Integer) ..] aliceUtxos) $ \(i, (_, output)) -> noteL ("Alice UTxO number " <> show i) output
  -- Ensuring that Alice has 2 utxos with quick values with the right amount
  aliceQuickValueExtracts <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP trueScript quick)
  assert "We properly extracted the quick token from Alice's utxos" $ aliceQuickValueExtracts == ((`HCons` HEmpty) <$> [4, 10])
  -- Ensuring that Alice has 2 utxos created with hashed datums with permanent
  -- values, and retrieving the typed content of those datums.
  aliceHashedDatums <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP falseScript permanent)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumKindAT % datumKindResolvedP)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumTypedAT @Integer)
  assert "We properly extracted more info from Alice's utxos" $
    aliceHashedDatums
      == [ HCons 5 (HCons NotResolved (HCons 15 HEmpty)),
           HCons 12 (HCons Resolved (HCons 10 HEmpty))
         ]

{--
So now that we've seen those features, let's clear out the field and
start using those UTxOs we've created.
Make sure the log is visible.
Speak about signatory and balancing.

Enable show consumed utxos

RUN IT

Remove line  alice `receives` Value (quickValue 4)

RUN IT

Balancing fails.
Add manual UTxOs for balancing using.

  txSkelOpts = def {txSkelOptBalancingUtxos = BalancingUtxosFromSet $ Set.fromList (fst <$> outputs)}

Notice, thanks to the "consumed utxos", that one UTxO with a datum has been used.
This makes sense considering the options we provided.
Maybe a better solution here would be to manually provide the UTxO to spend and preserve the datum in the residual payment.

  txSkelOuts =
    [ bob `receives` Value (quickValue 2),
      snd (outputs !! 6) & txSkelOutValueL % valueAssetClassAmountL trueScript quick %~ (+ (-2))
    ],
  txSkelIns = Map.singleton (fst (outputs !! 6)) emptyTxSkelRedeemer

--}

demoRunFirstTransaction :: ExtendedStagedMockChain (Reader Environment) ()
demoRunFirstTransaction = do
  alice <- asks alice >>= define "alice"
  bob <- asks bob >>= define "bob"
  trueScript <- asks trueScript >>= define "trueScript"
  falseScript <- asks falseScript >>= define "falseScript"
  permanent <- asks permanent >>= define "permanent"
  quick <- asks quick >>= define "quick"
  let permanentValue = review (valueAssetClassAmountP falseScript permanent)
      quickValue = review (valueAssetClassAmountP trueScript quick)
  outputs <-
    forceOutputs $
      replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` Value (permanentValue 3) <&&> InlineDatum (3 :: Integer),
             alice `receives` Value (permanentValue 5) <&&> HiddenHashedDatum (5 :: Integer),
             -- alice `receives` Value (quickValue 4),
             alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` Value (permanentValue 12) <&&> VisibleHashedDatum (12 :: Integer),
             alice `receives` InlineDatum (0 :: Integer)
           ]
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        -- txSkelOuts = [bob `receives` Value (quickValue 2)],
        txSkelOuts =
          [ bob `receives` Value (quickValue 2),
            snd (outputs !! 6) & txSkelOutValueL % valueAssetClassAmountL trueScript quick %~ (+ (-2))
          ],
        txSkelIns = Map.singleton (fst (outputs !! 6)) emptyTxSkelRedeemer
        -- txSkelOpts = def {txSkelOptBalancingUtxos = BalancingUtxosFromSet $ Set.fromList (fst <$> outputs)}
      }

{--
Now, what if we want to quickly change the target of the payment from bob to wallet 3?
Of course, we don't want to manually change the nominal transaction.

Let's use our first tweak !
--}

tweakedDemoRunFirstTransaction :: ExtendedStagedMockChain (Reader Environment) ()
tweakedDemoRunFirstTransaction = withTweak demoRunFirstTransaction $ do
  carol <- define "carol" (wallet 3)
  setTweak (txSkelOutsL % traversed % txSkelOutOwnerL) (UserPubKey carol)

{--
We can see that carol hit the jackpot ! She got both UTxOs for herself. Something
went wrong, all we wanted to do was redirect the output initially targeted to bob.
Let's do better and use ... the DatumHijackingAttack !

Take a look at the various parameters.

RUN IT !
--}

dhDemoRunFirstTransaction :: ExtendedStagedMockChain (Reader Environment) ()
dhDemoRunFirstTransaction = withTweak demoRunFirstTransaction $ do
  carol <- define "carol" $ wallet 3
  bob <- asks bob
  datumHijackingAttack $ ownedByDatumHijackingParams bob carol

{--
Show that the Skeleton has been labelled in the log

This is already nice, but attempting a DH attack when no script is involved is a bit lame.
--}

tests :: TestTree
tests = testCooked "Full staged run" $ mustSucceedTest @_ @StagedEffs (return ())
