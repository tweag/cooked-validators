# CHEATSHEET

Welcome to `cooked-validators`' **Cheatsheet** 

You will find here code snippets to help you use the library! Among other
things, you will Learn how to:
- write transactions
- fetch information from the blockchain
- submit these transactions for validation as part of a trace
- run those traces in tests or in a `repl` environement!

While this is not a complete tutorial, this document should be helpful to new
users to get accustomed to `cooked-validators` as well as old users looking to
see how things have evolved and are currently done in `cooked-validators`!

- [CHEATSHEET](#cheatsheet)
- [Mockchain runs](#mockchain-runs)
  - [Mockchains](#mockchains)
  - [Traces](#traces)
    - [Define a trace](#define-a-trace)
    - [Execute the trace in a `cabal repl`](#execute-the-trace-in-a-cabal-repl)
    - [Use the trace as part of a test](#use-the-trace-as-part-of-a-test)
  - [Initial distributions of UTxOs](#initial-distributions-of-utxos)
    - [Creation](#creation)
    - [Usage](#usage)
- [Miscellaneous mockchain capabilities](#miscellaneous-mockchain-capabilities)
  - [Wallets](#wallets)
  - [Aliases](#aliases)
    - [Aliases for static data](#aliases-for-static-data)
    - [Aliases for dynamic data](#aliases-for-dynamic-data)
  - [Notes](#notes)
  - [Assertions](#assertions)
- [Basic mockchain capabilities](#basic-mockchain-capabilities)
  - [Time handling](#time-handling)
    - [Querying time](#querying-time)
    - [Jumping in time](#jumping-in-time)
  - [Utxos queries](#utxos-queries)
    - [Direct queries](#direct-queries)
    - [Searches](#searches)
- [Transactions](#transactions)
  - [Transaction skeletons](#transaction-skeletons)
    - [Defining a transaction skeleton](#defining-a-transaction-skeleton)
    - [Submitting transaction skeletons for validation](#submitting-transaction-skeletons-for-validation)
  - [Signatories](#signatories)
  - [Outputs](#outputs)
    - [Building payments:](#building-payments)
    - [Adjusting payments:](#adjusting-payments)
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

# Mockchain runs

## Mockchains

A `mockchain` is an abstraction of the Cardano blockchain. `cooked-validators`
provides several monadic environments to instantiate this concept:
- `DirectMockChain`: basic capabilities of the mockchain
- `StagedMockChain`: basic capabilities of the mockchain, with the addition of
  branching and temporal modifications. This is the go-to environement !
- `FullMockChain`: all effects available, including low-level effects such as
  builtin-errors, to be used for maximum level of granularity
- `ExtendedStagedMockChain eff`: same as `StagedMockChain` with additional
  custom effects embedded in `eff`, to work in your own dedicated environement !

## Traces 

### Define a trace

A trace is a sequence of instructions in one of our `mockchain` instances:

* In a fixed existing `mockchain` intance:
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
myTrace :: (Members '[MockChainLog, MockChainRead, MyFirstEff, ...] effs) => Sem effs ()
myTrace = do
  ...
```

### Execute the trace in a `cabal repl`

* With a default mockchain state: 
```haskell
printCooked $ runMockChainDef myTrace
```
* With a custom mockchain state: 
```haskell
printCooked $ runMockChain myState myTrace
```
* With a custom initial list of payments (or `InitialDistribution`):
```haskell
printCooked $ runMockChainFromInitDist myPaymentsList myTrace
```
* With a default configuration (initial state, initial list of payments, and custom function on returned value): 
```haskell
printCooked $ runMockChainFromConf myConf myTrace
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
* Manually customize the:
  * initial distribution: `withInitDist myPaymentsList`
  * pretty printing options: `withPrettyOpts myOptions`
  * expectations on the log: `withLogProp myLogProp`
  * expectations on the resulting state: `withStateProp myStateProp`
  * expectations on the successful outcome: `withSuccessProp mySuccessProp`
  * expectations on the returned value: `withResultProp myResultProp`
  * expectations on the number of results: `withSizeProp mySizeProp`
  * expectations on the failure output: `withFailureProp myFailureProp`
  * expectations on the possible error: `withErrorProp myError prop`
* Wrap the test in a quickcheck/tasty test case:
  * `testCooked "myTastyTest" $ myTastyTest`
  * `testCookedQC "myQuickCheckTest" $ myQuickCheckTest`
* Example:
```haskell
testCooked $ 
  mustSucceedTest myTrace 
    `withInitDist` myListOfPayments -- giving a custom initial distribution
    `withSizeProp` isOfSize 3 -- expecting 3 outputs
    `withResultProp` (== 42) -- each returned value should be 42
    `withLogProp` happened "MCLogAddedReferenceScript" -- a reference script was automatically added
    `withStateProp` possesses alice myAssetClass 10 -- alice possesses 10 custom tokens in the resulting state
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
  [ wallet 3 `receives` Value (ada 6)
  , fooTypedValidator `receives` Value (myToken 6) <&&> InlineDatum fooTypedDatum
  , wallet 2 `receives` Value (ada 2) <&&> VisibleHashedDatum fooDatum
  , wallet 1 `receives` Value (ada 10) <&&> ReferenceScript fooValidator <&&> StakingCredential cred
  ]
```
### Usage

* In a test:
```haskell
testCooked "foo" $ mustSucceedTest foo `withInitDist` myInitiDist
```
* In a `repl`:
```haskell
printCooked $ runMockChainFromInitDist myInitDist foo
```
* Within a trace:
```haskell
myTrace = do
  forceOutputs myInitDist
  ...
```

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

Aliases for static data can be defined outside a mockchain run, in the pretty options direclty:
```haskell
walletNames :: [(Wallet, String)]
walletNames = [(wallet 1, "Alice"), (wallet 2, "Bob"), (wallet 3, "Carie")]

mintingPolicyNames :: [(Wallet, MintingPolicy)]
mintingPolicyNames = [(nftCurrencySymbol, "NFT"), (customCoinsCurrencySymbol, "Custom Coins")]

validatorNames :: [(Wallet, Validator)]
validatorNames = [(fooValidator, "Foo") ]

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
of the mockchain run.

* Named assertions: `assert`
* Assertions with default name "Assertion": `assert'`

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

### Jumping in time

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

## Utxos queries 

The mockchain state contains, among other elements, the unspent outputs, which
can be queried/searched for. They are associated with the payments that created
them, in the form of a `TxSkelOut`.

### Direct queries

* Retrieve all UTxOs available in the mockchain state:
```haskell
myTrace = do
  ...
  knownUtxos <- allUtxos -- knownUtxos :: [(TxOutRef, TxSkelOut)]
  ...
```

* Retrieve all UTxOs at a given address:
```haskell
myTrace = do
  ...
  aliceUtxos <- utxosAt alice -- aliceUtxos :: [(TxOutRef, TxSkelOut)]
  ...
```

* Fetch the `TxSkelOut` associated with a `TxOutRef`:
```haskell
myTrace = do
  ...
  txSkelOut <- txSkelOutByRef myTxOutRef
  ...
```

* Get a certain part of a `TxSkelOut` from a `TxOutRef` and an optic
```haskell
myTrace = do
  ...
  -- A value is always present, use 'viewByRef'
  value <- viewByRef txSkelOutValueL myTxOutRef
  -- A datum of a given type might not be present, use 'previewByRef'
  Just typedDatum <- previewByRef (txSkelOutDatumL % txSkelOutDatumTypedAT @MyDatumType) myTxOutRef
  ...
```

### Searches

Utxo searches are lists of UTxOs that can be manipulated conveniently. 

1. Utxo searches are created using:
  * `beginSearch` from a monadic call returning a list of UTxOs such as `allUtxos`
  * `beginSearchP` from a pure call returning a list of UTxOs

2. Some existing UTxO searches are provided builtin such as `utxosAtSearch`,
   `allUtxosSearch` or `txSkelOutByRefSearch`.

3. UTxO searches can be filtered using...
  * a monadic predicate over a `TxSkelOut` using `ensure`
  * a pure predicate over a `TxSkelOut` using `ensurePure`
  * the presence, or absence of the target of an affine fold using either
    `ensureAFoldIs` or `ensureAFoldIsn't` respectively.

4. Some builtin filters are provided, such as `ensureVanillaOutputs` or
   `ensureProperReferenceScript`.
   
5. UTxO searches can be refined by extracting sub-elements from the `TxSkelOut`s using...
  * a monadic function which might fail (returning `Maybe b`), which will
    extract `b` when it exists, or remove the output when it does not.
  * several variants: `extractPure`, `extractAFold`, ...
  
6. UTxO searches are made of the targetted outputs, alongside any piece of
   information that was extracted from them, in a type retaining way. Pieces of
   these searches can be retrieved, using `getOutputs`, `getExtracts`, ...
   
7. Example:
```haskell
myTrace = do
  ...
  oRefs <- getTxOutRefs $ -- only retrieve the TxOutRef
    utxosAtSearch alice $ -- use UTxOs at alice's address only
	  ensureAFoldIs (txSkelOutValueL % filtered (banana 1 `Api.leq`)) -- only take UTxOs containing at least a banana
  ...
```

# Transactions

## Transaction skeletons

Transaction skeletons are abstractions over real transactions, allowing to
define minimal content proper to each concern. 

### Defining a transaction skeleton

A transaction skeleton is composed of transaction elements and transaction
options. It is built upon a transaction skeleton template. Each field can then
be overridden.

```haskell
myTxSkel = txSkelTemplate 
  { txSkelIns = ...,
	txSkelOuts = ...,
	txSkelOpts = ...,
	...
  }
```

### Submitting transaction skeletons for validation

Transaction skeletons can be submitted for validation and...

* ... get the validated Cardano transaction
```haskell
myTrace = do
  ...
  cardanoTx <- validateTxSkel myTxSkel
  ...
```

* ... get the generated `TxOutRef`s: `validateTxSkel'`

* ... ignore any returned value: `validateTxSkel_`

## Signatories

Transaction can be signed with one of more wallets. They will both be part of
the required and actual signers of the transaction.

```haskell
txSkelTemplate
  { ...
    txSkelSignatories = txSkelSignatoriesFromList [wallet 1, ...]
    ...
  }
```

Transactions can also be signed by anything that has a pubkey. They will only be part of the required signatories, and a private key will be required later on.
  
```haskell
instance Script.ToPubKeyHash MyType where
  toPubKeyHash = ... 

myUser1 myUser2 :: MyType
myUser1 = ...
myUser2 = ...

txSkelTemplate
  { ...
    txSkelSignatories = signatoryPubKey <$> [myUser1, myUser2, ...]
    ...
  }
```

## Outputs

### Building payments:

There are several payable elements, attached to a recipient to create a payment:
* A simple value to a wallet: 
```haskell
wallet 3 `receives` Value (ada 3)
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

### Adjusting payments:

Payments can automatically be adjusted in terms of minimal ADA requirements:
* allow min ADA adjustment, by providing a value: ```party `receives` (Value (myToken 5))```
* allow min ADA adjustment, by providing no value: ```party `receives` (Datum myDatum)```
* forbid min ADA adjustment: ```party `receives` (FixedValue $ ada 10) ```

### Attaching payments to transactions

Payments are given in the transaction using the `txSkelOuts` field:
```haskell
txSkelTemplate
  { ...
    txSkelOuts = [party1 `receives` payment1, party2 `receives` payment2, ...]
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
txSkelTemplate
  { ...
    txSkelIns = Map.fromList 
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
txSkelTemplate
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
txSkelTemplate
  { ...
    txSkelInsReference = Set.fromList [txOutRef1, txOutRef2, ...]
    ...
  }
```

## Collaterals

Collaterals are usually selected by `cooked-validators` automatically, but can
also be provided manually.

* From first signer (default):
```
txSkelTemplate
  { ...
     txSkelSignatories = [signatory1, signatory2],
    ...
  }
```

* From another wallet:
```
txSkelTemplate
  { ...
    txSkelSignatories = [TxSkelSignatory user1 ... , TxSkelSignatory user2 ...],
	txSkelOpts = def {txSkelOptCollateralUtxos = CollateralUtxosFromUser user2}
    ...
  }
```

* From a direct UTxO list (make sure the owner of these utxo sign the transaction):
```
txSkelTemplate
  { ...
	txSkelOpts = def {txSkelOptCollateralUtxos = CollateralUtxosFromSet (Set.fromList [txOutRef1, txOutRef2]) user2}
    ...
  }
```

## Proposal procedures

### Tamper with the official constitution script

* Set the official constitution scripts

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
  currentConstitution <- getConstitutionScript
  ...
```

### Attach a Proposal Procedure to a transaction

* Using the builtin constructor for proposals.

```haskell
txSkelTemplate
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
txSkelTemplate
  { ...
    txSkelProposals =
      [ simpleProposal
          (wallet 1)
          (ParameterChange [FeePerByte 100, FeeFixed 1_000])
          & fillConstitution myConstitutionScript
          & set txSkelProposalAnchorL (simpleURLAnchor "https://www.tweag.io/")
      ]
    ...
  } 
```

* Auto filling of the current official constitution script.

Leave a witnessed proposal's constitution unset (i.e. `Nothing`). During
transaction generation the current official constitution script (the one set
with `setConstitutionScript`) is automatically filled in with an empty
redeemer, logging an `MCLogAutoFilledConstitution` event.

## Withdrawals

* Automatic withdrawal of the available rewards

```haskell 
txSkelTemplate
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
    txSkelTemplate
      { txSkelWithdrawals = txSkelWithdrawalsFromList 
	      [ Withdrawal (UserPubKey myWithdrawingPeer) (Just $ Api.Lovelace 2_000_000),
		    ...
	      ]
	    ...
      }
```	

## Certificates

* Build certificate actions

```haskell
myCertificateAction = CommitteResign ...
myCertificateAction2 = DRepUpdate ...
```

* Add certificates to transactions. Make sure the kind of certificate
  corresponds to the kind of allowed user.

```haskell
txSkelTemplate
  { txSkelCertificates = 
      [ TxSkelCertificate myUser myCertificateAction,
	    pubKeyCertificate myPubKey myCertificateAction1,
		scriptCertificate myScript myRedeemer myCertificateAction2
	  ],
    ... 
  }
```

## Balancing

### Pick which user provides UTxOs to balance a transaction

* First signatory (default):
```haskell
txSkelTemplate
  { ...
    txSkelSignatories = [signatory1, signatory2]
    ...
  }
```

* Another signatory:
```haskell
txSkelTemplate
  { ...
    txSkelSignatories = [signatory1, signatory2],
    txSkelOpts = def {txSkelOptBalancingPolicy = BalanceWith (wallet 2)}
    ...
  }
```

### Do not automatically balance

```haskell
txSkelTemplate
  { ...
    txSkelOpts = def {txSkelOptBalancingPolicy = DoNotBalance}
    ...
  }
```

# Tweaks

## Defining tweaks

Tweaks are state-aware modifications applied to transactions, which can fail. In
a tweak, one can:
* issue any action available in the encompassing mockchain environment
* modify a `TxSkel` through dedicated primitives
Tweaks cannot be used in `DirectMockChain`.

Example:
```haskell
myTweak :: TypedTweak effs ()
myTweak = do
  myActionInEffs
  ...
  setTxSkel ...
  overTweak ...
  myOtherActionInEffs
  ...
```

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
  there 3 myTrace myTweak
  ...
```

## Examples

* Tamper with inputs and outputs 
```haskell
foo = do
  addOutputTweak $ bazValidator `receives` bazPayment
  removeOutputTweak (\(Pays out) -> somePredicate out)
  addInputTweak somePkTxOutRef emptyTxSkelRedeemer
  removeInputTweak (\txOutRef redeemer -> somePredicate txOutRef redeemer)
```

* Tamper with signatories
```haskell
foo = do
   addSignatoriesTweak [signatory1, signatory2]
   replaceFirstSigner signatory3
   removeSigners [signatory2]
```

* Using optics in tweaks
```haskell
foo = C.overTweak
  (txSkelOutsL % ix 1 % txSkelOutValueL) -- Value of first output
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
  never myTweak myTrace
  ...
```

### Custom formulas

Custom LTL formulas can be used for more advanced use case. 

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
