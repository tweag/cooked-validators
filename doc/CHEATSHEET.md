# CHEATSHEET

Welcome to `cooked-validators`' **Cheatsheet** 

You will find here code snippets to help you use the library! Among other
things, you will Learn how to:
- write transactions
- fetch information from the blockchain
- submit these transactions for validation as part of a trace
- run those traces in tests or in a `repl` environment!

While this is not a complete tutorial, this document should be helpful to new
users to get accustomed to `cooked-validators` as well as old users looking to
see how things have evolved and are currently done in `cooked-validators`!

- [CHEATSHEET](#cheatsheet)
- [Mockchain runs](#mockchain-runs)
  - [Effects](#effects)
  - [Mockchains](#mockchains)
  - [Traces](#traces)
    - [Define a trace](#define-a-trace)
    - [Execute the trace in a `cabal repl`](#execute-the-trace-in-a-cabal-repl)
    - [Use the trace as part of a test](#use-the-trace-as-part-of-a-test)
  - [Initial distributions of UTxOs](#initial-distributions-of-utxos)
    - [Creation](#creation)
    - [Usage](#usage)
- [Running against a Cardano node](#running-against-a-cardano-node)
  - [Blockchains](#blockchains)
  - [Running a trace on a node](#running-a-trace-on-a-node)
  - [Persisting the chain index](#persisting-the-chain-index)
  - [Writing node-friendly traces](#writing-node-friendly-traces)
- [Miscellaneous mockchain capabilities](#miscellaneous-mockchain-capabilities)
  - [Wallets](#wallets)
  - [Aliases](#aliases)
    - [Aliases for static data](#aliases-for-static-data)
    - [Aliases for dynamic data](#aliases-for-dynamic-data)
  - [Notes](#notes)
  - [Assertions](#assertions)
  - [Pretty printing](#pretty-printing)
- [Basic mockchain capabilities](#basic-mockchain-capabilities)
  - [Time handling](#time-handling)
    - [Querying time](#querying-time)
    - [Jumping in time](#jumping-in-time)
  - [Protocol parameters](#protocol-parameters)
  - [Utxos queries](#utxos-queries)
    - [Direct queries](#direct-queries)
    - [Searches](#searches)
  - [Overriding the mockchain state](#overriding-the-mockchain-state)
- [Transactions](#transactions)
  - [Transaction skeletons](#transaction-skeletons)
    - [Defining a transaction skeleton](#defining-a-transaction-skeleton)
    - [Submitting transaction skeletons for validation](#submitting-transaction-skeletons-for-validation)
  - [Transaction options](#transaction-options)
    - [Option templates](#option-templates)
    - [Handling failures](#handling-failures)
    - [Other options](#other-options)
  - [Signatories](#signatories)
  - [Outputs](#outputs)
    - [Building payments:](#building-payments)
    - [Adjusting payments:](#adjusting-payments)
    - [Owners known only by their hash](#owners-known-only-by-their-hash)
    - [Attaching payments to transactions](#attaching-payments-to-transactions)
  - [Redeemers](#redeemers)
  - [Inputs](#inputs)
  - [Minted value](#minted-value)
  - [Reference inputs](#reference-inputs)
  - [Collaterals](#collaterals)
  - [Proposal procedures](#proposal-procedures)
    - [Tamper with the official constitution script](#tamper-with-the-official-constitution-script)
    - [Attach a Proposal Procedure to a transaction](#attach-a-proposal-procedure-to-a-transaction)
  - [Withdrawals](#withdrawals)
  - [Certificates](#certificates)
    - [Stake pools](#stake-pools)
  - [Balancing](#balancing)
    - [Pick which user provides UTxOs to balance a transaction](#pick-which-user-provides-utxos-to-balance-a-transaction)
    - [Do not automatically balance](#do-not-automatically-balance)
- [Tweaks](#tweaks)
  - [Defining tweaks](#defining-tweaks)
  - [Tweaks: modify single transactions](#tweaks-modify-single-transactions)
  - [Examples](#examples)
  - [Temporal modifications](#temporal-modifications)
    - [Builtin formulas](#builtin-formulas)
    - [Custom formulas](#custom-formulas)
- [Attacks](#attacks)
  - [Datum hijacking](#datum-hijacking)
  - [Token duplication](#token-duplication)
  - [Datum tampering](#datum-tampering)
  - [Redeemer tampering](#redeemer-tampering)
  - [Validity tampering](#validity-tampering)
  - [Peer tampering](#peer-tampering)
  - [Outputs reordering](#outputs-reordering)

# Mockchain runs

## Effects

The capabilities of a chain are expressed as [Polysemy] effects, all
re-exported from `Cooked.Effect`. Each of them comes with a `runMockChain*`
interpreter (emulated chain) and, when meaningful, a `runBlockChain*`
interpreter (real Cardano node):
- `Query`: fetching UTxOs (`allUtxos`, `utxosAt`, `utxoByRef`, ...), the
  constitution script and the current rewards
- `Params`: protocol parameters, network id, era history, system start and
  deposits
- `Time`: querying and waiting for slots
- `Validate`: submitting skeletons for validation (`validateTxSkel`, ...)
- `Override`: modifying the chain state directly (`forceOutputs`, `setParams`,
  `setConstitutionScript`). Only available on the mockchain.
- `Misc`: aliases, notes and assertions
- `Log`: internal log of events (`ChainLogEntry`)
- `Submit`: low-level submission of a Cardano `Transaction`

Traces can be written against any set of these effects:
```haskell
myTrace :: (Members '[Validate, Query, Time, Fail] effs) => Sem effs ()
```

## Mockchains

A `mockchain` is an abstraction of the Cardano blockchain. `cooked-validators`
provides several monadic environments to instantiate this concept:
- `DirectMockChain`: basic capabilities of the mockchain (`Validate`,
  `Override`, `Query`, `Time`, `Misc` and `Fail`)
- `StagedMockChain`: basic capabilities of the mockchain, with the addition of
  branching and temporal modifications. This is the go-to environment !
- `FullMockChain`: all effects available, including low-level effects such as
  builtin-errors, `Params` or `Log`, to be used for maximum level of
  granularity
- `ExtendedStagedMockChain eff`: same as `StagedMockChain` with additional
  custom effects embedded in `eff`, to work in your own dedicated environment !

Their respective stacks of effects are `DirectMockChainEffs`,
`StagedMockChainEffs`, `FullMockChainEffs` and `ExtendedStagedMockChainEffs
eff`.

## Traces 

### Define a trace

A trace is a sequence of instructions in one of our `mockchain` instances:

* In a fixed existing `mockchain` instance:
```haskell
myTrace :: [Direct|Staged|Full]MockChain ()
myTrace = do
  ...
```

* In a mockchain extended with a single user-defined effect:
```haskell
myTrace :: ExtendedStagedMockChain MyEff ()
myTrace = do
  ...
  actionInMyEff
  ...
```

* In a mockchain extended with several effects (using `Polysemy`'s bundle capability):
```haskell
myTrace :: ExtendedStagedMockChain (Bundle '[MyFirstEff, MySecondEff,..]) ()
myTrace = do
  ...
  sendBundle $ actionInMyFirstEff
  ...
  sendBundle $ actionInMySecondEff
  ...
```

* In a direct set of custom or builtin effects:
```haskell
myTrace :: (Members '[Log, Query, MyFirstEff, ...] effs) => Sem effs ()
myTrace = do
  ...
```

### Execute the trace in a `cabal repl`

* With a default mockchain state and no initial payments: 
```haskell
printCooked $ runMockChainDef myTrace
```
* With the default initial distribution (4 UTxOs of 100 Ada for wallets 1 to 4):
```haskell
printCooked $ runMockChainFromInitDistTemplate myTrace
```
* With a custom initial emulator state and chain index (no initial
  distribution, raw result):
```haskell
runMockChain (def :: EmulatorState) (def :: ChainIndex) myTrace
```
* With a custom initial list of payments (or `InitialDistribution`):
```haskell
printCooked $ runMockChainFromInitDist myPaymentsList myTrace
```
* With a custom configuration (initial emulator state, initial chain index,
  initial list of payments, and custom function on returned value):
```haskell
printCooked $ runMockChainFromConf myConf myTrace
-- or by tweaking the default configuration
printCooked $ runMockChainFromConf (mockChainConfTemplate {mccInitialDistribution = myDist}) myTrace
```

### Use the trace as part of a test

* Expect a successful outcome from a trace:
  `mustSucceedTest myTrace`
* Expect a failure from a trace:
  `mustFailTest myTrace`
* Expect a failure within a specific validation phase:
  `mustFailInPhase1Test myTrace` or `mustFailInPhase2Test myTrace`
* Expect a failure within a specific validation phase with a specific error message:
  `mustFailInPhase1WithMsgTest expectedMessage myTrace` or `mustFailInPhase2WithMsgTest expectedMessage myTrace`
* Expect a given number of outcomes:
  `mustSucceedWithSizeTest 3 myTrace` or `mustFailWithSizeTest 3 myTrace`
* Manually customize the:
  * initial distribution: `withInitDist myPaymentsList`
  * pretty printing options: `withPrettyOpts myOptions`
  * expectations on the log: `withLogProp myLogProp`
  * expectations on the resulting state: `withStateProp myStateProp`
  * expectations on the successful outcome: `withSuccessProp mySuccessProp`
  * expectations on the returned value: `withResultProp myResultProp`
  * expectations on the number of results: `withSizeProp mySizeProp`
  * expectations on the failure output: `withFailureProp myFailureProp`
  * expectations on the possible error: `withErrorProp (\(err :: ChainError) -> ...)`
* Builtin properties:
  * on failures: `isPhase1Failure`, `isPhase2Failure`,
    `isPhase1FailureWithMsg`, `isPhase2FailureWithMsg` (phase 2 failures are
    script evaluation failures reported in `CEExUnitsFailures`, phase 1
    failures are ledger rejections reported in `CESubmissionFailures` and
    other non-script errors reported in `CEExUnitsFailures`)
  * on the number of outcomes: `isOfSize`, `isAtLeastOfSize`, `isAtMostOfSize`
  * on the log: `happened "CLogEntryName"`, `didNotHappen "CLogEntryName"`
  * on the final state: `possesses`, `isAtAddress`
* Wrap the test in a quickcheck/tasty test case:
  * `testCooked "myTastyTest" $ myTastyTest`
  * `testCookedQC "myQuickCheckTest" $ myQuickCheckTest`
  * with the default initial distribution: `testCookedFromInitDistTemplate`
    and `testCookedQCFromInitDistTemplate`
* Example:
```haskell
testCooked "myTest" $ 
  mustSucceedTest myTrace 
    `withInitDist` myListOfPayments -- giving a custom initial distribution
    `withSizeProp` isOfSize 3 -- expecting 3 outputs
    `withResultProp` (== 42) -- each returned value should be 42
    `withLogProp` happened "CLogAddedReferenceScript" -- a reference script was automatically added
    `withSuccessProp` possesses alice myAssetClass 10 -- alice possesses 10 custom tokens in the resulting state
```
* Example with an expected error:
```haskell
testCooked "myFailingTest" $
  mustFailTest myTrace
    `withErrorProp` \case
      CEUnknownOutRefs _ -> testSuccess
      _ -> testFailure
```

## Initial distributions of UTxOs

Initial distributions of funds are lists of payment used to populate the
blockchain without needing a transaction.

### Creation

* With values only
```haskell
initDist :: InitialDistribution
initDist = distributionFromList $
    [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
    , (wallet 2 , [ ada 10 ])
    , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
    ]
```
* With arbitrary payments
```haskell
initDist :: InitialDistribution
initDist = 
  [ wallet 3 `receives` AdaValue 6 -- exactly 6 Ada, not subject to min-Ada adjustment
  , fooTypedValidator `receives` Value (myToken 6) <&&> InlineDatum fooTypedDatum
  , wallet 2 `receives` Value (ada 2) <&&> VisibleHashedDatum fooDatum -- Ada amount adjusted to min-Ada if needed
  , wallet 1 `receives` AdaValue 10 <&&> ReferenceScript fooValidator <&&> StakingCredential cred
  ]
```
* The default initial distribution, `initialDistributionTemplate`, gives 4
  UTxOs of 100 Ada to each of the first 4 wallets.

### Usage

* In a test:
```haskell
testCooked "foo" $ mustSucceedTest foo `withInitDist` myInitiDist
```
* In a `repl`:
```haskell
printCooked $ runMockChainFromInitDist myInitDist foo
```
* Within a trace (only on the mockchain, through the `Override` effect):
```haskell
myTrace = do
  createdUtxos <- forceOutputs myInitDist -- createdUtxos :: Utxos
  forceOutputs_ myOtherInitDist -- ignoring the created UTxOs
  ...
```

# Running against a Cardano node

Besides the emulated mockchain, `cooked-validators` can run traces against a
real Cardano node (for instance a local testnet), using the `Cooked.BlockChain`
backend. The same traces can then be used for testing on the emulator and for
deploying on a real network, as long as they do not use mockchain-only effects
such as `Override`.

## Blockchains

Two environments are provided:
- `DirectBlockChain`: the go-to environment, with effects `Validate`, `Query`,
  `Time`, `Misc` and `Fail`
- `FullBlockChain`: all effects available, including low-level ones (`Submit`,
  `Params`, `Log`, errors, `IO`, ...)

Their respective stacks of effects are `DirectBlockChainEffs` and
`FullBlockChainEffs`.

## Running a trace on a node

The connection to the node is described by a `Cardano.LocalNodeConnectInfo`:
```haskell
import Cardano.Api qualified as Cardano

connectInfo :: Cardano.LocalNodeConnectInfo
connectInfo =
  Cardano.LocalNodeConnectInfo
    { Cardano.localConsensusModeParams = Cardano.CardanoModeParams (Cardano.EpochSlots 21600),
      Cardano.localNodeNetworkId = Cardano.Testnet (Cardano.NetworkMagic 42),
      Cardano.localNodeSocketPath = Cardano.File "/path/to/node.socket"
    }
```

* Run a trace and display its result and the resulting UTxOs:
```haskell
main :: IO ()
main = runBlockChainFromConfTemplate connectInfo (myTrace :: DirectBlockChain ())
```

* Run a trace with a custom configuration:
```haskell
main :: IO ()
main =
  runBlockChainFromConf
    BlockChainConf
      { bccConnectInfo = connectInfo,
        bccChainIndexSource = def, -- the outputs known before the run (here none)
        bccChainIndexTarget = Nothing, -- where to save the final chain index, if anywhere
        bccPrettyOpts = myPrettyOpts,
        bccFunOnResult = displayBlockChainResult -- or any 'RawBlockChainReturn a -> IO b'
      }
    myTrace
```

* Start from the default configuration: `blockChainConfDefaultTemplate
  connectInfo`, which is what `runBlockChainFromConfTemplate` uses.

## Persisting the chain index

The chain index (the outputs known to cooked, with their typed datums and
owners) can be loaded from and saved to a JSON file, so that successive runs
against a node build upon each other. See [CODECS](CODECS.md) for details.

* Load the chain index from a file before the run, and save it back after:
```haskell
main :: IO ()
main =
  runBlockChainFromConf
    (blockChainConfWithDefaultRegistriesTemplate connectInfo "chain-index.json")
    myTrace
```

* Custom types appearing in outputs (datums, owners) must have a `HasTypeKey`
  instance and be registered to be loaded back:
```haskell
instance HasTypeKey MyDatum -- or with an explicit key: typeKey = "my-datum"

main :: IO ()
main =
  runBlockChainFromConf
    ( blockChainConfWithExtraRegistryTemplate
        connectInfo
        "chain-index.json"
        (extendDatumRegistry (mkRegistry [Codec @MyDatum]) emptyRegistries)
    )
    myTrace
```

* Use exactly the given registries, without the default ones:
  `blockChainConfWithGivenRegistryTemplate connectInfo path registries`

* Choose the source of the initial chain index manually:
```haskell
myConf =
  (blockChainConfDefaultTemplate connectInfo)
    { -- path, registries, accept a missing file, accept a malformed file
      bccChainIndexSource = ChainIndexFromFile "in.json" defaultRegistries True False,
      -- or directly: bccChainIndexSource = DirectChainIndex myChainIndex
      bccChainIndexTarget = Just "out.json"
    }
```

* Directly in `IO`: `loadChainIndex path registries allowMissing
  allowMalformed`, and `saveChainIndex path chainIndex`, which keeps a backup of
  the previous file in `path.bak`.

* Beware: private keys of wallets and signatories owning outputs are stored in
  plain text in these files.

## Writing node-friendly traces

* Build skeletons from `txSkelNodeTemplate`, whose options do not halt on
  failures and limit the number of balancing UTxOs (see [Transaction
  options](#transaction-options)).
* Sign with actual keys, for instance read from a Cardano signing key file
  (see [Signatories](#signatories)):
```haskell
main = do
  alice <- signatoryFromFile @Cardano.GenesisUTxOKey "utxo1.skey"
  runBlockChainFromConfTemplate connectInfo $
    validateTxSkel_ $
      txSkelNodeTemplate
        { txSkelOutputs = [bob `receives` AdaValue 10],
          txSkelSignatories = [alice]
        }
```
* UTxO queries are sent to the node, and completed with the content stored in
  the chain index (outputs created during the run, or provided through
  `bccChainIndexSource`). Outputs only discovered on the node may have their
  datum or owning script known only by their hash, in which case spending them
  requires additional care (see [Owners known only by their
  hash](#owners-known-only-by-their-hash)).
* On a node, time flows on its own and `waitNSlots` and the like actually
  suspend the thread, while on the mockchain time only changes when explicitly
  requested.

# Miscellaneous mockchain capabilities

`cooked-validators` provides various quality of life capabilities to be used
during a mockchain run.

## Wallets

Wallets are dummy peers that can be used for testing purposes. There are 10
wallets, from `wallet 1` to `wallet 10`.

```haskell
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3

alice :: Wallet 
alice = wallet 1

aliceAddress :: Address
aliceAddress = Script.toAddress alice

alicePKHash :: PubKeyHash 
alicePKHash = Script.toPubKeyHash alice
```

Other wallet queries:
* `walletSK`
* `walletPK`
* `walletStakingPK`
* ...

## Aliases

It is possible to define aliases for everything that is hashable, for pretty
printing/debugging purposes at the end of a run.

### Aliases for static data

Aliases for static data can be defined outside a mockchain run, in the pretty options directly:
```haskell
walletNames :: [(Wallet, String)]
walletNames = [(wallet 1, "Alice"), (wallet 2, "Bob"), (wallet 3, "Carie")]

mintingPolicyNames :: [(Api.CurrencySymbol, String)]
mintingPolicyNames = [(nftCurrencySymbol, "NFT"), (customCoinsCurrencySymbol, "Custom Coins")]

validatorNames :: [(VScript, String)]
validatorNames = [(toVScript fooValidator, "Foo") ]

pcOpts :: C.PrettyCookedOpts
pcOpts =
  addHashNames
    ( C.hashNamesFromList walletNames
        <> C.hashNamesFromList mintingPolicyNames
        <> C.hashNamesFromList validatorNames
    )
    def
```

### Aliases for dynamic data

Aliases for dynamic data (depending on on-chain elements such as `TxOutRef`s)
can be defined directly within a mockchain run:

* Result of a pure call:
```haskell
myTrace = do
  ...
  myScript <- define "myScript" $ scriptFromTxOutRef txOutRef
  ...
```

* Result of an impure call:
```haskell
myTrace = do
  ...
  myScript <- defineM "myValidator" $ do
    ...
    return $ scriptFromTxOutRef txOutRef
  ...
```

* Without using the result: `define_` and `defineM_`

## Notes 

Notes correspond to user logs during a mockchain run. We can take note of...
* Anything that can be pretty printed, alongside some pretty printing options
  using `note`.
* Anything that can be pretty printed, while using the current pretty options
  for rendering using `noteP`.
* Anything that can be pretty printed as a list, with a title, using `noteL`.
* Anything that can be shown, using `noteW`
* A string, using `noteS`.

## Assertions

Assertions can be made anywhere in a mockchain run. Assertions will be evaluated
during the run, used for testing purposes, and displayed in the final printing
of the mockchain run. Their name follows the same conventions as notes:

* Named by a rendering function: `assert (\opts -> prettyCookedOpt opts myThing) (x == y)`
* Named by anything that can be pretty printed: `assertP myThing (x == y)`
* Named by a title and a list of pretty printable elements: `assertL "Title" myList (x == y)`
* Named by anything that can be shown: `assertW myThing (x == y)`
* Named by a string: `assertS "x equals y" (x == y)`
* With default name "Assertion": `assert' (x == y)`

## Pretty printing

* Print the result of a run with default options: `printCooked`
* Print the result of a run with custom options: `printCookedOpt myOptions`
* Some useful `PrettyCookedOpts` fields:
  * `pcOptPrintTxSkelOpts`: print the options of each skeleton (hidden by default)
  * `pcOptPrintLog`, `pcOptPrintNotebook`, `pcOptPrintAssertions`: print the
    log, the notes and the assertions
  * `pcOptPrintRemainingUTxOs`, `pcOptPrintConsumedUTxOs`: print the final UTxOs
  * `pcOptHashes`: hash printing options, including aliases (see above)
```haskell
printCookedOpt def {pcOptPrintTxSkelOpts = True} $ runMockChainDef myTrace
```

# Basic mockchain capabilities

## Time handling

Time can be queried/set within a mockchain run, to account for transaction with
specific validity interval/temporal constraints.

### Querying time

* The current slot can be queried directly:
```haskell
myTrace = do
  ...
  slot <- currentSlot
  ...
```

* An interval of ms can be queried as well, deduced from the slot:
```haskell
myTrace = do
  ...
  (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentMSRange
  ...
```

* No single ms point can be queried, as it does not exist!

* The slot containing a given ms point can be queried: `getEnclosingSlot`

* The ms range of a given slot can be queried: `slotToMSRange`

* Slot ranges before or after a given ms point can be computed, for validity
  ranges: `slotRangeBefore`, `slotRangeAfter`

### Jumping in time

Validating a transaction does not make time progress on the mockchain. Time has
to be explicitly moved forward:

* Waiting a certain amount of slots
```haskell
myTrace = do
  ...
  waitNSlots 3
  ...
```
* Similarly, we can:
  * jump to a given slot: `awaitSlot`
  * jump to the slot enclosing a given ms point: `awaitEnclosingSlot`
  * wait a certain amount of ms from the first ms of the current slot: `waitNMSFromSlotLowerBound`
  * wait a certain amount of ms from the last ms of the current slot: `waitNMSFromSlotUpperBound`

## Protocol parameters

The `Params` effect (available in `FullMockChain` and `FullBlockChain`) gives
access to:
* the ledger protocol parameters: `getParams`
* the network id: `getNetworkId`
* the era history and system start: `getEraHistory`, `getSystemStart`
* the various deposits: `govActionDeposit`, `dRepDeposit`,
  `stakeAddressDeposit`, `stakePoolDeposit`

## Utxos queries 

The chain index contains, among other elements, the unspent outputs, which
can be queried/searched for. They are associated with the payments that created
them, in the form of a `TxSkelOut`.

### Direct queries

Most queries come in three flavours:
* a base version, returning `Maybe` (or ignoring missing outputs)
* an `F` variant, failing (in `Fail`) when outputs are missing
* an `E` variant, throwing a `ChainError` (`CEUnknownOutRefs`) when outputs are
  missing, meant for internal use when `Error ChainError` is available

* Retrieve all UTxOs available in the mockchain state:
```haskell
myTrace = do
  ...
  knownUtxos <- allUtxos -- knownUtxos :: UtxoSearchResult, a Map TxOutRef (HList '[TxSkelOut])
  ...
```

* Retrieve all UTxOs at a given address (or anything convertible to an address):
```haskell
myTrace = do
  ...
  aliceUtxos <- utxosAt alice -- aliceUtxos :: UtxoSearchResult
  ...
```

* Retrieve the UTxOs associated with some `TxOutRef`s:
```haskell
myTrace = do
  ...
  someUtxos <- utxosByRef [txOutRef1, txOutRef2] -- ignores missing references
  someUtxos' <- utxosByRefF [txOutRef1, txOutRef2] -- fails on missing references
  ...
```

* Fetch the `TxSkelOut` associated with a `TxOutRef`:
```haskell
myTrace = do
  ...
  mTxSkelOut <- utxoByRef myTxOutRef -- mTxSkelOut :: Maybe TxSkelOut
  txSkelOut <- utxoByRefF myTxOutRef -- txSkelOut :: TxSkelOut
  ...
```

* Get a certain part of a `TxSkelOut` from a `TxOutRef` and an optic
```haskell
myTrace = do
  ...
  -- A value is always present, use 'viewByRef[F]'
  value <- viewByRefF txSkelOutValueL myTxOutRef
  -- A datum of a given type might not be present, use 'previewByRef[F]'
  Just typedDatum <- previewByRefF (txSkelOutDatumL % txSkelOutDatumTypedAT @MyDatumType) myTxOutRef
  ...
```

* Other queries:
  * the current official constitution script: `getConstitutionScript`
  * the current reward of a staking credential: `getCurrentReward`

### Searches

`allUtxos`, `utxosAt` and `utxosByRef` return a `UtxoSearchResult`, which is a
`Map TxOutRef (HList '[TxSkelOut])`. Such search results (from
`Cooked.Utilities.TypedSearch`) can be refined in a pipeline, extracting new
pieces of information from the outputs and filtering them along the way, while
retaining in the type the list of extracted elements.

1. UTxO searches can be filtered using...
  * a monadic predicate using `ensure`
  * a pure predicate using `ensurePure`
  * the presence, or absence of the target of an affine fold using either
    `ensureAFoldIs` or `ensureAFoldIsn't` respectively.

2. UTxO searches can be refined by extracting new elements from previously
   extracted ones (initially the `TxSkelOut`s), using...
  * a monadic function which might fail (returning `Maybe b`), which will
    extract `b` when it exists, or remove the output when it does not: `extract`
  * several variants: `extractPure`, `extractAFold`, `extractTotal`,
    `extractPureTotal`, `extractGetter`

3. Filters and extractions find the element they work on by its type (using
   `FetchByType`), so the type of the extracted elements should be unambiguous.

4. Pieces of the search results can be retrieved using...
  * `retrieveKeys`: the set of `TxOutRef`s
  * `retrieveAsList`: the list of `HList`s of extracted elements
  * `retrieveByType`, `retrieveByTypeAsList`: the extracted elements of a given
    type, in a map or in a list
  * `retrieveByTypeAndFold`: the fold of extracted elements of a given monoidal type
  * `retrieve`: an arbitrary function on the search result
  * `searchResultMapI`: an iso between a search with a single extracted element
    and a plain map, such as `Utxos`

5. Search results are maps, so usual `Map` functions can be used on them as well.
   
6. Examples:
```haskell
myTrace = do
  ...
  oRefs <- -- only retrieve the TxOutRefs
    utxosAt alice -- use UTxOs at alice's address only
      >>= ensureAFoldIs (txSkelOutValueL % filtered (banana 1 `Api.leq`)) -- only take UTxOs containing at least a banana
      >>= retrieveKeys
  datums <- -- retrieve all the integer datums at alice's address
    utxosAt alice
      >>= extractAFold (txSkelOutDatumL % txSkelOutDatumTypedAT @Integer)
      >>= retrieveByTypeAsList @Integer
  totalValue <- -- the sum of values at alice's address
    utxosAt alice
      >>= extractGetter txSkelOutValueL
      >>= retrieveByTypeAndFold @Api.Value
  ...
```

## Overriding the mockchain state

The `Override` effect is only available on the mockchain, and allows to modify
its state without submitting transactions:
* create outputs out of thin air: `forceOutputs`, `forceOutputs_`
* change the emulator parameters: `setParams` (takes an `Emulator.EmulatorParams`)
* change the official constitution script: `setConstitutionScript`

# Transactions

## Transaction skeletons

Transaction skeletons are abstractions over real transactions, allowing to
define minimal content proper to each concern. 

### Defining a transaction skeleton

A transaction skeleton is composed of transaction elements and transaction
options. It is built upon a transaction skeleton template. Each field can then
be overridden.

```haskell
myTxSkel = txSkelEmulatorTemplate 
  { txSkelInputs = ...,
	txSkelOutputs = ...,
	txSkelOpts = ...,
	...
  }
```

Three templates are available:
* `txSkelEmulatorTemplate`: an empty skeleton with options tailored for the
  emulator (`txSkelOptsEmulatorTemplate`)
* `txSkelNodeTemplate`: an empty skeleton with options tailored for a real node
  (`txSkelOptsNodeTemplate`)
* `txSkelTemplate myOptions`: an empty skeleton with custom options

### Submitting transaction skeletons for validation

Transaction skeletons can be submitted for validation and...

* ... get the adjusted skeleton, the submission failures (when not halting),
  the generated Cardano transaction and the created UTxOs
```haskell
myTrace = do
  ...
  (extendedTxSkel, submissionFailures, cardanoTx, createdUtxos) <- validateTxSkel myTxSkel
  ...
```

* ... get the created UTxOs (as `Utxos`, a `Map TxOutRef TxSkelOut`): `validateTxSkel'`

* ... get the created `TxOutRef`s, in order: `validateTxSkelL`
```haskell
myTrace = do
  ...
  (oRef1 : oRef2 : _) <- validateTxSkelL myTxSkel
  ...
```

* ... ignore any returned value: `validateTxSkel_`

## Transaction options

Options are stored in the `txSkelOpts` field of the skeleton, of type
`TxSkelOpts`, and accessible through optics (`txSkelOptsL % txSkelOpt...L`).

### Option templates

There is no default set of options. Instead, two templates are provided:
* `txSkelOptsEmulatorTemplate`: halts on every failure (execution units and
  submission), which is what we usually want when testing
* `txSkelOptsNodeTemplate`: does not halt on failures (they are only logged),
  and uses at most 10 UTxOs for balancing

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelOpts = txSkelOptsEmulatorTemplate {txSkelOptBalancingPolicy = DoNotBalance}
    ...
  }
```

### Handling failures

* Halt (or not) when script evaluation fails while computing execution units:
  `txSkelOptHaltOnExUnitsFailures`. When not halting, the failures are logged
  (`CELogExUnitsFailures`) and the transaction is generated with an invalid
  script validity flag, and thus submitted as a phase 2 failure.
* Halt (or not) when the transaction is rejected upon submission:
  `txSkelOptHaltOnSubmissionFailures`. When not halting, the failures are
  logged (`CELogSubmissionFailures`) and returned by `validateTxSkel`.

### Other options

* Balancing (see [Balancing](#balancing)): `txSkelOptBalancingPolicy`,
  `txSkelOptFeePolicy` (`AutoFeeComputation` or `ManualFee fee`),
  `txSkelOptBalanceOutputPolicy` (`AdjustExistingOutput` or
  `DontAdjustExistingOutput`), `txSkelOptBalancingUtxos`
  (`BalancingUtxosFromBalancingUser` or `BalancingUtxosFromSet utxos`) and
  `txSkelOptMaxNbOfBalancingUtxos`
* Collaterals (see [Collaterals](#collaterals)): `txSkelOptCollateralUtxos`
* Arbitrary modification of the generated transaction, applied right before
  submission: `txSkelOptModTx`, or `txSkelOptAddModTx` to compose with existing
  modifications
```haskell
myTxSkel {txSkelOpts = txSkelOptAddModTx Debug.Trace.traceShowId (txSkelOpts myTxSkel)}
```

## Signatories

Transaction can be signed with one of more wallets. They will both be part of
the required and actual signers of the transaction.

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelSignatories = txSkelSignatoriesFromList [wallet 1, ...] -- or: signatoryWallet <$> [wallet 1, ...]
    ...
  }
```

Transactions can also be signed by anything that has a pubkey. They will only be
part of the required signatories, and a private key will be required later on.
  
```haskell
instance Script.ToPubKeyHash MyType where
  toPubKeyHash = ... 

myUser1 myUser2 :: MyType
myUser1 = ...
myUser2 = ...

txSkelEmulatorTemplate
  { ...
    txSkelSignatories = signatoryPubKey <$> [myUser1, myUser2, ...]
    ...
  }
```

More generally, a signatory is a public key hash paired with an optional
Cardano signing key (`Maybe Cardano.ShelleyWitnessSigningKey`), which can be
built from any key with a `ToWitness` instance:

```haskell
mySignatory = TxSkelSignatory myPubKeyHash (Just $ P.Ledger.toWitness mySigningKey)
```

Signatories can be read from a Cardano signing key file (in `IO`):

```haskell
main = do
  signatory <- signatoryFromFile @Cardano.GenesisUTxOKey "path/to/key.skey"
  ...
```

Notes:
* Signatories are compared by public key hash only.
* Signatories can be used as owners of outputs: ``mySignatory `receives` AdaValue 10``.
* The optional signing key of a signatory can be accessed with
  `txSkelSignatoryPrivateKeyL`, or `txSkelSignatoryPrivateKeyAT` to focus on
  the key itself, and its public key hash with `txSkelSignatoryPubKeyHashL`.
* The cold key of a mock stake pool can sign pool certificates:
  `signatoryPoolCold myPool` (see [Stake pools](#stake-pools)).
* The witness of a signatory can be accessed with `txSkelSignatoryPrivateKeyL`
  and its signing key, if any, with `txSkelSignatorySigningKeyAF`.

## Outputs

### Building payments:

There are several payable elements, attached to a recipient to create a payment:
* A simple value to a wallet: 
```haskell
wallet 3 `receives` AdaValue 3
```
* A value and an inline datum to a script:
```haskell
fooScript `receives` (InlineDatum FooTypedDatum <&&> Value (myToken 4 <> lovelace 160_000))
```
* Hashed datums (visible to the transaction or hidden from it): 
```haskell
... <&&> (VisibleHashedDatum dat) -- resolved in the transaction
... <&&> (HiddenHashedDatum dat) -- unresolved in the transaction
```
* A reference script: 
```haskell
... <&&> (ReferenceScript dat)
```
* A staking credential:
```haskell
... <&&> (StakingCredential dat)`
```

Owners can be wallets, public key hashes, signatories, scripts (typed
validators, versioned validators, multi-purpose scripts...), script hashes or
users (`UserPubKey`, `UserScript`, `UserScriptHash`).

Typed datums must satisfy `DatumConstrs`: `Show`, `PrettyCooked`, `ToData`,
`FromData`, `Eq`, `Typeable` and `HasTypeKey`. Typed owners additionally need
`ToJSON`, `FromJSON` and `HasTypeKey` (constraints `PubKeyConstrs` and
`ScriptConstrs`). `HasTypeKey` has a default implementation (the fully
qualified type name), so an empty instance is usually enough:
```haskell
instance HasTypeKey MyDatum
```

### Adjusting payments:

Payments can automatically be adjusted in terms of minimal ADA requirements:
* allow min ADA adjustment, by providing a value: ```party `receives` (Value (myToken 5))```
* allow min ADA adjustment, by providing no value: ```party `receives` (InlineDatum myDatum)```
* forbid min ADA adjustment: ```party `receives` (FixedValue $ ada 10) ```
* forbid min ADA adjustment, with an amount of ADA: ```party `receives` AdaValue 10```
* forbid min ADA adjustment, with an amount of lovelace: ```party `receives` LovelaceValue 2_000_000```

### Owners known only by their hash

Outputs can exist on-chain whose datum or owning script is only known by its
hash. This typically happens with outputs discovered on a real node.
* Pay to a script only known by its hash:
```haskell
(myScriptHash :: Api.ScriptHash) `receives` AdaValue 10
```
* Spending such an output requires its redeemer to point to a reference input
  holding the actual script, otherwise the run fails with
  `CESpendingHashOnlyScript`:
```haskell
txSkelInputs = Map.singleton oRef (withReferenceInput myRedeemer oRefWithRefScript)
```
* An output can also hold a datum only known by its hash
  (`SomeTxSkelOutDatumHash datumHash`, set through `txSkelOutDatumL`).
  Spending such an output fails with `CESpendingHashOnlyDatum`.

### Attaching payments to transactions

Payments are given in the transaction using the `txSkelOutputs` field:
```haskell
txSkelEmulatorTemplate
  { ...
    txSkelOutputs = [party1 `receives` payment1, party2 `receives` payment2, ...]
    ...
  }
```

## Redeemers

Redeemers are provided whenever a script is invoked, regardless of its
purpose. `cooked-validators` will automatically fetch a proper reference inputs
when applicable.

* No redeemer, auto fill of reference script: `emptyTxSkelRedeemer`
* No redeemer, forbid auto fill of reference script: `emptyTxSkelRedeemerNoAutoFill`
* Some redeemer, auto fill of reference script: `someTxSkelRedeemer myRedeemer`
* Some redeemer, forbid auto fill of reference script: `someTxSkelRedeemerNoAutoFill myRedeemer`
* Attach a reference input manually (with a reference script): ``withReferenceInput txSkelRedeemer txOutRefContainingRefScript``
* Build the redeemer as a record manually:
```haskell
myRedeemer :: TxSkelRedeemer
myRedeemer =
  TxSkelRedeemer
    { txSkelRedeemerContent = red,
      txSkelRedeemerReferenceInput = Just txOutRefContainingRefScript,
      txSkelRedeemerAutoFill = False -- ignored if txSkelRedeemerReferenceInput /= Nothing
    }
```

## Inputs

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelInputs = Map.fromList 
	  [ (txOutRef1, someTxSkelRedeemer red), 
        (txOutRef2, withReferenceInput emptyTxSkelRedeemer txOutRef),
	    (txOutRef3, someTxSkelRedeemerNoAutoFill red2)
	  ]
    ...
  }
```

## Minted value

* Mint tokens: positive amount
* Burn tokens: negative amount
* Mint/Burn several different tokens from the same MP: `Mint fooPolicy myTxSkelRedeemer [("fooName", 3),("barName", -5)]`
* Mint a single kind of token for a given minting policy: `mint fooPolicy myTxSkelRedeemer "fooName" 5`
* Burn a single kind of token for a given minting policy: `burn barPolicy myTxSkelRedeemer "barName" 6`

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelMints = txSkelMintsFromList
      [ Mint ...,
        mint ...,
		burn ...,
		Mint ...
      ]
    ...
  }
```

## Reference inputs

* Within redeemers automatically `myTxSkelRedeemer`
* Within redeemers manually ``withReferenceInput myTxSkelRedeemer myRefInput``
* Additional reference inputs not bound to redeemers:
```haskell
txSkelEmulatorTemplate
  { ...
    txSkelReferenceInputs = Set.fromList [txOutRef1, txOutRef2, ...]
    ...
  }
```

## Collaterals

Collaterals are usually selected by `cooked-validators` automatically, but can
also be provided manually.

* From the balancing user, by default the first signatory (default):
```haskell
txSkelEmulatorTemplate
  { ...
     txSkelSignatories = [signatory1, signatory2],
    ...
  }
```

* From another wallet:
```haskell
txSkelEmulatorTemplate
  { ...
    txSkelSignatories = [TxSkelSignatory user1 ... , TxSkelSignatory user2 ...],
	txSkelOpts = txSkelOptsEmulatorTemplate {txSkelOptCollateralUtxos = CollateralUtxosFromUser user2}
    ...
  }
```

* From a direct UTxO list (make sure the owner of these utxo sign the transaction):
```haskell
txSkelEmulatorTemplate
  { ...
	txSkelOpts = txSkelOptsEmulatorTemplate {txSkelOptCollateralUtxos = CollateralUtxosFromSet (Set.fromList [txOutRef1, txOutRef2]) user2}
    ...
  }
```

## Proposal procedures

### Tamper with the official constitution script

* Set the official constitution scripts (mockchain only)

```haskell
do
  ...
  setConstitutionScript myScript
  ...
```

* Retrieve the official constitution script

```haskell
do
  ...
  mCurrentConstitution <- getConstitutionScript -- mCurrentConstitution :: Maybe VScript
  ...
```

### Attach a Proposal Procedure to a transaction

* Using the builtin constructor for proposals.

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelProposals =
      [ TxSkelProposal
          { -- The credential that should be used for the return account
            txSkelProposalReturnCredential = wallet 1,
            txSkelProposalGovernanceAction =
              TreasuryWithdrawals $
                Map.fromList
                  [ (toCredential $ wallet 1, Api.Lovelace 100),
                    (toCredential $ wallet 2, Api.Lovelace 10_000)
                  ],
            -- The constitution witness, only relevant for witnessed
            -- governance actions; use 'Nothing' for unwitnessed ones
            txSkelProposalConstitution = Just (UserRedeemedScript myConstitutionScript myTxSkelRedeemer),
            -- An optional anchor (use 'simpleURLAnchor' to build one)
            txSkelProposalAnchor = Nothing
          }
      ]
    ...
  }
```

* Using smart constructors and (optional) helpers.

```haskell 
txSkelEmulatorTemplate
  { ...
    txSkelProposals =
      [ simpleProposal
          (wallet 1)
          (ParameterChange [FeePerByte 100, FeeFixed 1_000])
          & fillConstitutionWhenEmpty myConstitutionScript
          & set txSkelProposalAnchorL (simpleURLAnchor "https://www.tweag.io/")
      ]
    ...
  } 
```

* Auto filling of the current official constitution script.

Leave a witnessed proposal's constitution unset (i.e. `Nothing`). During
transaction generation the current official constitution script (the one set
with `setConstitutionScript`) is automatically filled in with an empty
redeemer, logging a `CLogAutoFilledConstitution` event.

## Withdrawals

* Automatic withdrawal of the available rewards

```haskell 
txSkelEmulatorTemplate
  { txSkelWithdrawals = txSkelWithdrawalsFromList 
      [ scriptWithdrawal myWithdrawingScript myTxSkelRedeemer,
	    pubKeyWithdrawal myWithdrawingPubKey,
	    ...
      ]
    ...
  }
```	

* Manual withdrawal of a certain amount (for testing purposes only)

```haskell 
    txSkelEmulatorTemplate
      { txSkelWithdrawals = txSkelWithdrawalsFromList 
	      [ Withdrawal (UserPubKey myWithdrawingPeer) (Just $ Api.Lovelace 2_000_000),
		    ...
	      ]
	    ...
      }
```	

## Certificates

* Build certificate actions:
  * staking: `StakingRegister`, `StakingUnRegister`, `StakingDelegate
    delegatee`, `StakingRegisterDelegate delegatee` (where `delegatee` is an
    `Api.Delegatee`: `Api.DelegStake poolId`, `Api.DelegVote drep` or
    `Api.DelegStakeVote poolId drep`)
  * DReps: `DRepRegister`, `DRepUpdate`, `DRepUnRegister`
  * pools (public keys only): `PoolRegister vrfHash`, `PoolRetire slot`
  * committee: `CommitteeRegisterHot credential`, `CommitteeResign`

```haskell
myCertificateAction1 = StakingRegister
myCertificateAction2 = DRepUpdate
```

* Add certificates to transactions. Make sure the kind of certificate
  corresponds to the kind of allowed user.

```haskell
txSkelEmulatorTemplate
  { txSkelCertificates =
      [ TxSkelCertificate myUser myCertificateAction,
        pubKeyCertificate myPubKey myCertificateAction1,
        scriptCertificate myScript myRedeemer myCertificateAction2
      ],
    ...
  }
```

* Deposits required by registrations are automatically accounted for during
  balancing.

### Stake pools

`Cooked.Utilities.Pools` provides deterministic mock stake pools (`Pool`), with
their keys and identifiers:
* get a pool from its number: `pool 3`, or the default ones: `knownPools`
* identifiers as `Api.PubKeyHash`: `poolId`, `poolVRFHash`, and back with
  `poolIdToNumber`, `poolIdToPool`
* Bech32 identifier: `poolIdBech32`
* keys: `poolColdPK`, `poolColdSK`, `poolVRFPK`, `poolVRFSK`, `poolKESPK`,
  `poolKESSK`

* Register a pool (signed by its cold key with `signatoryPoolCold`), then
  delegate stake to it:

```haskell
registerAndDelegate :: DirectMockChain ()
registerAndDelegate = do
  let p = pool 1
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelSignatories = [signatoryWallet (wallet 1), signatoryPoolCold p],
        txSkelCertificates = [pubKeyCertificate (poolId p) (PoolRegister $ poolVRFHash p)]
      }
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelSignatories = [signatoryWallet (wallet 1)],
        txSkelCertificates =
          [ pubKeyCertificate (wallet 1) (StakingRegisterDelegate $ Api.DelegStake $ poolId p)
          ]
      }
```

* Retire a registered pool at the epoch containing a given slot:
  `pubKeyCertificate (poolId p) (PoolRetire slot)`

## Balancing

### Pick which user provides UTxOs to balance a transaction

* First signatory (default):
```haskell
txSkelEmulatorTemplate
  { ...
    txSkelSignatories = [signatory1, signatory2]
    ...
  }
```

* Another signatory:
```haskell
txSkelEmulatorTemplate
  { ...
    txSkelSignatories = [signatory1, signatory2],
    txSkelOpts = txSkelOptsEmulatorTemplate {txSkelOptBalancingPolicy = BalanceWith (wallet 2)}
    ...
  }
```

### Do not automatically balance

```haskell
txSkelEmulatorTemplate
  { ...
    txSkelOpts = txSkelOptsEmulatorTemplate {txSkelOptBalancingPolicy = DoNotBalance}
    ...
  }
```

# Tweaks

## Defining tweaks

Tweaks are state-aware modifications applied to transactions, which can fail. In
a tweak, one can:
* issue any action available in the tweak's stack of effects (for
  `StagedMockChain`, those are `Misc`, `Query`, `Time` and `Fail`)
* modify a `TxSkel` through dedicated primitives
Tweaks cannot be used in `DirectMockChain`.

Each mockchain environment has an associated tweak type: `StagedTweak`,
`FullTweak` and `ExtendedStagedTweak eff`.

Example:
```haskell
myTweak :: StagedTweak ()
myTweak = do
  myActionInEffs
  ...
  setTweak someOptic someValue
  overTweak someOptic someModification
  myOtherActionInEffs
  ...
```

The tweak primitives are re-exported from `Cooked.Tweak`, and split into focused
modules: `Query` (`viewTweak`, `toListOfTweak`, `previewTweak`, ...), `Update`
(`setTweak`, `overTweak`, `traverseTweak`, ...), `Insert` (`insertLastTweak`,
`insertAtTweak`, `insertInTweak`, ...), `Remove` (`removeIfTweak`,
`removeAtTweak`, ...), `Modify` (`modifyTweak` and its branching helpers), and
`Guard` (`guardTweak`, `condTweak`, `labelled`, ...).

## Tweaks: modify single transactions

* Apply a tweak on a given transaction
```haskell
myTrace = do
  ...
  myModifiedTxSkel <- execTweak myTxSkel myTweak
  ...
```

* Apply a tweak on the first transaction of a trace
```haskell
myTrace = do
  ...
  withTweak myTrace myTweak
  ...
```

* Apply a tweak on the nth transaction of a trace (0-indexed)
```haskell
myTrace = do
  ...
  there 3 myTweak myTrace
  ...
```

## Examples

* Tamper with inputs and outputs
```haskell
foo = do
  insertLastTweak txSkelOutputsL $ bazValidator `receives` bazPayment
  removeIfTweak txSkelOutputsL somePredicate
  insertAtTweak txSkelInputsL somePkTxOutRef emptyTxSkelRedeemer
  removeAtTweak txSkelInputsL somePkTxOutRef
```

* Tamper with signatories
```haskell
foo = do
   appendAfterTweak txSkelSignatoriesL [signatoryPubKey user1, signatoryPubKey user2]
   setTweak (txSkelSignatoriesL % _head) (signatoryPubKey user3)
   removeIfTweak txSkelSignatoriesL somePredicate
```

* Using optics in tweaks
```haskell
foo = C.overTweak
  (txSkelOutputsL % ix 1 % txSkelOutValueL) -- Value of first output
  (<> assetClassValue bazAssetClass 10) -- Add 10 baz tokens
```

## Temporal modifications

Tweaks can be deployed "on-time" using various temporal combinators inspired by
Linear Temporal Logics formulas (LTL) in traces composed of several transactions. 

### Builtin formulas

* Apply a tweak on all transactions in a trace, where it must never fail:
```haskell
myTrace = do
  ...
  everywhere myTweak myTrace
  ...
```

* Apply a tweak whenever possible, branching for each position where it applies:
```haskell
myTrace = do
  ...
  somewhere myTweak myTrace
  ...
```

* Apply a tweak whenever it applies, skipping transactions when it does not:
```haskell
myTrace = do
  ...
  whenAble myTweak myTrace
  ...
```

* Apply a tweak to all transactions with a given text label:
```haskell
myTrace = do
  ...
  whenAble (labelled' myLabel myTweak) myTrace
  ...
```

* Ensure a tweak cannot be applied in any transaction of a trace:
```haskell
myTrace = do
  ...
  nowhere myTweak myTrace
  ...
```

### Custom formulas

Custom LTL formulas (from `Cooked.MockChain.Ltl`) can be used for more advanced use case. 

* Wrap a tweak into an atomic LTL formula:
```haskell
myAtom = LtlAtom $ UntypedTweak myTweak
```

* Build an LTL formula from atomic modifications: 
```haskell
myFormula = myAtom1 `LtlAnd` (myAtom2 `LtlOr` (myAtom3 `ltlImplies` myAtom4))
```

* Modify a computation with the built formula:
```haskell
myTrace = do
  ...
  modifyLtl myFormula myTrace
  ...
```

# Attacks

Attacks are predefined, state-aware tweaks that try to uncover vulnerabilities by
systematically modifying transactions. Being tweaks, they are deployed on a trace
using the temporal combinators (`somewhere`, `everywhere`, `there`, ...) or
applied to a single skeleton with `execTweak`/`withTweak`. Each attack takes a
parameters record, adds a descriptive label to the modified transactions, and
returns the parts it modified, as they were before modification. All attacks are
re-exported from `Cooked`.

Most attacks take a `Branching` policy describing how to branch when several foci
are eligible:
* `OneBranchForAllFoci`: modify all eligible foci in a single transaction
* `OneBranchPerFoci`: modify exactly one eligible focus per branch
* `OneBranchPerSubset`: one branch per non-empty subset of eligible foci
* `Manual f`: branch according to a custom function on the focused indexes

## Datum hijacking

Redirects selected outputs to a given thief, to check that a script cannot be
tricked into paying an attacker.

```haskell
myTrace = do
  ...
  somewhere
    (datumHijackingAttack $ scriptsDatumHijackingParams OneBranchForAllFoci thief)
    myTrace
  ...
```

* Select which outputs to redirect with a params builder:
  * from an affine fold: `defaultDatumHijackingParams branching optic thief`
  * from a predicate: `outPredDatumHijackingParams branching outPred thief`
  * by owner type: `typedByDatumHijackingParams @OldOwner branching thief`
  * by specific owner: `ownedByDatumHijackingParams branching oldOwner thief`
  * owned by any script: `scriptsDatumHijackingParams branching thief`
  * by datum type: `datumOfDatumHijackingParams @Dat branching thief`
* Restrict which of the redirectable outputs are actually redirected (counted
  from the left, starting at zero) by overriding `dhpIndexPred`:
```haskell
(scriptsDatumHijackingParams OneBranchForAllFoci thief) { dhpIndexPred = (== 1) }
```

## Token duplication

Adds extra minted tokens and redirects the surplus to an attacker.

```haskell
myTrace = do
  ...
  somewhere
    (tokenDuplicationAttack $ existingAssetClassTokenDuplicationParams (\_ _ n -> n + 1) attacker)
    myTrace
  ...
```

* Params builders:
  * from an explicit list of `Mint`s: `anyMintTokenDuplicationParams mints attacker`
  * per existing currency: `existingCurrencyTokenDuplicationParams (\script -> [(tName, n)]) attacker`
  * per existing asset class: `existingAssetClassTokenDuplicationParams (\script tName n -> newN) attacker`

## Datum tampering

Modifies the datums of a transaction.

```haskell
myTrace = do
  ...
  somewhere
    (datumTamperingAttack $ overloadDatumTamperingParams OneBranchForAllFoci someOptic (const True))
    myTrace
  ...
```

* Params builders:
  * target all datums with a modification: `allDatumTamperingParams branching modification`
  * append dummy data to the targeted datums: `overloadDatumTamperingParams branching optic indexPred`

## Redeemer tampering

Modifies redeemers of a given type, at one or all of the five redeemer positions.

```haskell
myTrace = do
  ...
  somewhere
    (redeemerTamperingAttack $ spendingRedeemerTamperingParams OneBranchForAllFoci (\r -> Just (r + 1)))
    myTrace
  ...
```

* Params builders, one per position plus a catch-all, each taking a branching
  policy and a modification `a -> f b`:
  * `spendingRedeemerTamperingParams`
  * `mintingRedeemerTamperingParams`
  * `proposingRedeemerTamperingParams`
  * `withdrawingRedeemerTamperingParams`
  * `certifyingRedeemerTamperingParams`
  * `allRedeemerTamperingParams`

## Validity tampering

Tampers with the validity interval of a transaction.

```haskell
myTrace = do
  ...
  somewhere
    (validityTamperingAttack $ lowerStrictValidityTamperingParams (\s -> [s + 1]))
    myTrace
  ...
```

* Params builders, each taking a tampering function embedded in a `Foldable`
  `Alternative` (e.g. a list):
  * lower bound: `lowerExtendedValidityTamperingParams`, `lowerStrictValidityTamperingParams`
  * upper bound: `upperExtendedValidityTamperingParams`, `upperStrictValidityTamperingParams`
  * both bounds: `bothExtendedValidityTamperingParams`, `bothStrictValidityTamperingParams`
  * whole interval: `intervalValidityTamperingParams`

The `Extended` variants target the possibly-infinite bounds (`Maybe Slot`), while
the `Strict` variants target finite bounds and fail on infinite ones.

## Peer tampering

Replaces a peer with another everywhere it appears in a skeleton, to uncover
permission breaches between peers with different privileges.

```haskell
myTrace = do
  ...
  somewhere
    (peerTamperingAttack $ singlePeerTamperingParams existingPeer newPeer)
    myTrace
  ...
```

* Params builders:
  * a single replacement: `singlePeerTamperingParams existingPeer newPeer`
  * replace the balancing user: `balancingPeerTamperingParams newPeer`
  * a pure list of replacements: `purePeerTamperingParams branching [(existingPeer, [newPeer1, newPeer2])]`

## Outputs reordering

Reorders the outputs of a transaction, which can uncover vulnerabilities in
scripts relying on the outputs order (including some double satisfaction cases).

```haskell
myTrace = do
  ...
  somewhere (outputsReorderingAttack Shuffle) myTrace
  ...
```

* Reordering policies:
  * swap two outputs: `Swap i j`
  * move an output: `Move i j`
  * try every permutation (except the identity): `Shuffle`
  * do it manually: `ManualReordering (\outs -> [reorderings])`
