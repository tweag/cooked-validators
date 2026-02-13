{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.StagedRun where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.InlineDatums
import Plutus.Script.Utils.V3.Generators
import Plutus.Script.Utils.V3.Typed
import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3 qualified as Api
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
        { pcOptPrintLog = False,
          pcOptHashes = def, -- {pcOptHashNames = mempty},
          pcOptPrintConsumedUTxOs = False,
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

Start with the following running function

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
        { pcOptPrintLog = False,
          pcOptHashes = def {pcOptHashNames = mempty},
          pcOptPrintConsumedUTxOs = False,
          pcOptPrintRemainingUTxOs = True
        }
    )
    . runMockChainDef

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
  trueScript <- define "trueScript" $ trueMPScript @()
  quick <- define "quick" $ Api.TokenName "quick"
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (quickValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (quickValue 5) <&&> HiddenHashedDatum (5 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` Value (quickValue 12) <&&> VisibleHashedDatum (12 :: Integer),
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
  trueScript <- asks trueScript >>= define "trueScript"
  quick <- asks quick >>= define "quick"
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (quickValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (quickValue 5) <&&> HiddenHashedDatum (15 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (25 :: Integer),
           alice `receives` Value (quickValue 12) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` InlineDatum (0 :: Integer)
         ]

instance InterpretAlone (Reader Environment) where
  runInterpretAlone = runReader initialEnvironment

{--
RUN IT

HIDE THE LOG

Now, let's make some assertions, searches and traces
--}

demoRunSearches :: ExtendedStagedMockChain (Reader Environment) ()
demoRunSearches = do
  alice <- asks alice >>= define "alice"
  trueScript <- asks trueScript >>= define "trueScript"
  quick <- asks quick >>= define "quick"
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  outputs <-
    forceOutputs $
      replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` Value (quickValue 3) <&&> InlineDatum (3 :: Integer),
             alice `receives` Value (quickValue 5) <&&> HiddenHashedDatum (5 :: Integer),
             alice `receives` Value (quickValue 4),
             alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` InlineDatum (0 :: Integer)
           ]
  noteS "We have given a few assets to Alice at the beginning of the run"
  -- Ensuring that "Alice" got 10 utxos out of the "forceOutputs" call
  aliceUtxos <-
    beginSearchP outputs
      & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
  assert "Alice has the right amount of utxos" $ length aliceUtxos == 9
  forM_ (zip [(1 :: Integer) ..] aliceUtxos) $ \(i, (_, output)) -> noteL ("Alice UTxO number " <> show i) output
  -- Ensuring that Alice has 2 utxos with quick values with the right amount
  aliceQuickValueExtracts <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP trueScript quick)
  assert "We properly extracted the quick tokens from Alice's utxos" $ aliceQuickValueExtracts == ((`HCons` HEmpty) <$> [3, 5, 4, 10])
  -- Ensuring that Alice has 2 utxos created with hashed datums with permanent
  -- values, and retrieving the typed content of those datums.
  aliceHashedDatums <-
    getExtracts $
      beginSearchP outputs
        & ensureAFoldIs (txSkelOutOwnerL % userTypedAF @Wallet % filtered (== alice))
        . extractAFold (txSkelOutValueL % valueAssetClassAmountP trueScript quick)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumKindAT % datumKindResolvedP)
        . extractAFold (txSkelOutDatumL % txSkelOutDatumTypedAT @Integer)
  assert "We properly extracted more info from Alice's utxos" $
    aliceHashedDatums
      == [ HCons 5 (HCons NotResolved (HCons 5 HEmpty)),
           HCons 10 (HCons Resolved (HCons 10 HEmpty))
         ]

{--
So now that we've seen those features, let's clear out the field and
start using those UTxOs we've created.
Speak about signatory and balancing.

Make sure the log is visible.
Enable show consumed utxos

RUN IT

--}

demoRunFirstTransaction :: ExtendedStagedMockChain (Reader Environment) ()
demoRunFirstTransaction = do
  alice <- asks alice >>= define "alice"
  bob <- asks bob >>= define "bob"
  trueScript <- asks trueScript >>= define "trueScript"
  quick <- asks quick >>= define "quick"
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ $
    replicate 4 (alice `receives` Value (ada 10))
      ++ [ alice `receives` Value (quickValue 3) <&&> InlineDatum (3 :: Integer),
           alice `receives` Value (quickValue 5) <&&> HiddenHashedDatum (5 :: Integer),
           alice `receives` Value (quickValue 4),
           alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (10 :: Integer),
           alice `receives` InlineDatum (0 :: Integer)
         ]
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelOuts = [bob `receives` Value (quickValue 2)]
      }

{--

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

demoRunFirstTransaction2 :: ExtendedStagedMockChain (Reader Environment) ()
demoRunFirstTransaction2 = do
  alice <- asks alice >>= define "alice"
  bob <- asks bob >>= define "bob"
  trueScript <- asks trueScript >>= define "trueScript"
  quick <- asks quick >>= define "quick"
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  outputs <-
    forceOutputs $
      replicate 4 (alice `receives` Value (ada 10))
        ++ [ alice `receives` Value (quickValue 3) <&&> InlineDatum (3 :: Integer),
             alice `receives` Value (quickValue 5) <&&> HiddenHashedDatum (5 :: Integer),
             -- alice `receives` Value (quickValue 4),
             alice `receives` Value (quickValue 10) <&&> VisibleHashedDatum (10 :: Integer),
             alice `receives` Value (quickValue 12) <&&> VisibleHashedDatum (12 :: Integer),
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

Let's start fresh from a new trace, and have some scripts be executed first.
We can invoke a minting policy for the sake of it.
--}

demoRunFirstScripts :: StagedMockChain ()
demoRunFirstScripts = do
  alice <- define "alice" $ wallet 1
  trueScript <- define "trueScript" $ trueMPScript @()
  quick <- define "quick" $ Api.TokenName "quick"
  inlineScript <- define "inlineScript" requireInlineDatumInOutputValidator
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_ [alice `receives` Value (ada 100)]
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelMints = txSkelMintsFromList [mint trueScript () quick 2],
        txSkelOuts =
          [ inlineScript `receives` Value (quickValue 1),
            trueScript `receives` Value (quickValue 1)
          ]
      }

{--
We can see that the fee for the transaction is .396 ADA. This is not that huge
but maybe we can do even better. How about we use a reference script?
--}

demoRunReferenceScript :: StagedMockChain ()
demoRunReferenceScript = do
  alice <- define "alice" $ wallet 1
  trueScript <- define "trueScript" $ trueMPScript @()
  quick <- define "quick" $ Api.TokenName "quick"
  inlineScript <- define "inlineScript" requireInlineDatumInOutputValidator
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_
    [ alice `receives` Value (ada 100),
      alice `receives` ReferenceScript trueScript
    ]
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelMints = txSkelMintsFromList [mint trueScript () quick 2],
        txSkelOuts =
          [ inlineScript `receives` Value (quickValue 1),
            trueScript `receives` Value (quickValue 1)
          ]
      }

{--
We can see that cooked automatically attached the reference script input.
Not too bad, even for a small script like the always true script, we now
only have to pay .192 ADA

Of course this can be disabled. And manual utxos can be provided.

Now let's try to consume those UTxOs we've created at the script addresses.

--}

demoRunScriptUtxos :: StagedMockChain ()
demoRunScriptUtxos = do
  alice <- define "alice" $ wallet 1
  trueScript <- define "trueScript" $ trueMPScript @()
  quick <- define "quick" $ Api.TokenName "quick"
  inlineScript <- define "inlineScript" requireInlineDatumInOutputValidator
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_
    [ alice `receives` Value (ada 100),
      alice `receives` ReferenceScript trueScript
    ]
  (isORef, isOutput) : (tsORef, tsOutput) : _ <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelSignatories = txSkelSignatoriesFromList [alice],
          txSkelMints = txSkelMintsFromList [mint trueScript () quick 2],
          txSkelOuts =
            [ inlineScript `receives` Value (quickValue 1),
              trueScript `receives` Value (quickValue 1)
            ]
        }

  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelIns = Map.singleton tsORef emptyTxSkelRedeemer,
        txSkelOuts = [tsOutput]
      }

  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelIns = Map.singleton isORef emptyTxSkelRedeemer,
        txSkelOuts = [isOutput]
      }

{--
It does not work ! We have a phase 2 validation error, because the inline validator
requires ... an inline datum in its continuing output. Let's fix this.
--}

demoRunScriptUtxosFixed :: StagedMockChain ()
demoRunScriptUtxosFixed = do
  alice <- define "alice" $ wallet 1
  trueScript <- define "trueScript" $ trueMPScript @()
  quick <- define "quick" $ Api.TokenName "quick"
  inlineScript <- define "inlineScript" requireInlineDatumInOutputValidator
  let quickValue = review (valueAssetClassAmountP trueScript quick)
  forceOutputs_
    [ alice `receives` Value (ada 100),
      alice `receives` ReferenceScript trueScript
    ]
  (isORef, isOutput) : (tsORef, tsOutput) : _ <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelSignatories = txSkelSignatoriesFromList [alice],
          txSkelMints = txSkelMintsFromList [mint trueScript () quick 2],
          txSkelOuts =
            [ inlineScript `receives` Value (quickValue 1),
              trueScript `receives` Value (quickValue 1)
            ]
        }

  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelIns = Map.singleton tsORef emptyTxSkelRedeemer,
        txSkelOuts = [tsOutput]
      }

  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelIns = Map.singleton isORef emptyTxSkelRedeemer,
        txSkelOuts = [set txSkelOutDatumL (SomeTxSkelOutDatum () Cooked.Inline) isOutput],
        txSkelLabels = Set.fromList [label "Spending inlineScript"]
      }

{--
From here onwards, deactivate the log and the consumed UTXOs

Let spice things up a little bit.

We notice that spending the inline validator only requires an inline datum, no
matter the content of the utxo.

Let's label the interesting transaction (WE DO IT) and target it for a nice attack.
--}

demoRunInlineDatumHijacking :: StagedMockChain ()
demoRunInlineDatumHijacking =
  (`whenAble` demoRunScriptUtxosFixed) $ labelled' "Spending inlineScript" $ do
    void $ datumHijackingAttack (ownedByDatumHijackingParams requireInlineDatumInOutputValidator (wallet 1))
    overTweak txSkelOutsL ((requireInlineDatumInOutputValidator `receives` InlineDatum ()) :)

tests :: TestTree
tests = testCooked "Full staged run" $ mustSucceedTest @_ @StagedEffs (return ())
