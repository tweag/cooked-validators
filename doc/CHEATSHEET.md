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
    - [Querrying time](#querrying-time)
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
  - [Redeemers](#redeemers)
  - [Inputs](#inputs)
  - [Minted value](#minted-value)
  - [Reference inputs](#reference-inputs)
  - [Balancing](#balancing)
    - [Pick which user provides UTxOs to balance a transaction](#pick-which-user-provides-utxos-to-balance-a-transaction)
    - [Do not automatically balance](#do-not-automatically-balance)
  - [Collaterals](#collaterals)
  - [Proposal procedures](#proposal-procedures)
    - [Tamper with the official constitution script](#tamper-with-the-official-constitution-script)
    - [Attach a Proposal Procedure to a transaction](#attach-a-proposal-procedure-to-a-transaction)
  - [Withdrawals](#withdrawals)
  - [Certificates](#certificates)
- [Transaction modifications](#transaction-modifications)
  - [Tweaks: modify transactions](#tweaks-modify-transactions)
    - [Apply a modification](#apply-a-modification)
    - [Add or remove inputs and outputs](#add-or-remove-inputs-and-outputs)
    - [Modify signers](#modify-signers)
    - [Modify skeleton (inputs, outputs, options, etc.) using lenses](#modify-skeleton-inputs-outputs-options-etc-using-lenses)

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
* Expect a failure within a specific validation phase/with a specific error message:
  `mustFailInPhase[1|2](withMessage?) (expectedMessage?) myTrace`
* Manually customize the:
  * initial distribution: `withInitiDist myPaymentsList`
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

Time can be querried/set within a mockchain run, to account for transaction with
specific validity interval/temporal constraints.

### Querrying time

* The current slot can be querried directly:
```haskell
myTrace = do
  ...
  slot <- currentSlot
  ...
```

* An interval of ms can be querried as well, deduced from the slot:
```haskell
myTrace = do
  ...
  (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentMSRange
  ...
```

* No single ms point can be querried, as it does not exist!

* The slot containing a given ms point can be querried: `getEnclosingSlot`

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

* A simple value to a wallet: ```wallet 3 `receives` Value (ada 3)```
* A value and an inline datum to a script: ```fooTypedValidator `receives` (InlineDatum FooTypedDatum <&&> Value (myToken 4 <> lovelace 160_000))```
* Hashed datums (visible to the transaction or hidden from it): `... <&&> (VisibleHashedDatum dat)` or `... <&&> (HiddenHashedDatum dat)`
* A reference script: `(... <&&> ReferenceScript dat)`
* A staking credential: `(... <&&> StakingCredential dat)`

```haskell
txSkelTemplate
    { ...
      txSkelOuts = [party1 `receives` payment1, party2 `receives` payment2, ...]
      ...
    }
```

* allow min ADA adjustment, by providing a value: ```party `receives` (Value (myToken 5))```
* allow min ADA adjustment, by providing no value: ```party `receives` (Datum myDatum)```
* forbid min ADA adjustment: ```party `receives` (FixedValue $ ada 10) ```

## Redeemers

* No redeemer, auto fill of reference script: `emptyTxSkelRedeemer`
* No redeemer, forbid auto fill of reference script: `emptyTxSkelRedeemerNoAutoFill`
* Some redeemer, auto fill of reference script: `someTxSkelRedeemer myRedeemer`
* Some redeemer, forbid auto fill of reference script: `someTxSkelRedeemerNoAutoFill myRedeemer`
* Attach a reference input manually (with a reference script): ``txSkelRedeemer `withReferenceInput` txOutRefContainingRefScript``
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
        (txOutRef2, emptyTxSkelRedeemer `withReferenceInput` txOutRef),
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
* Within redeemers manually ``myTxSkelRedeemer `withReferenceInput` myRefInput``
* Additional reference inputs not bound to redeemers:
```haskell
txSkelTemplate
    { ...
      txSkelInsReference = Set.fromList [txOutRef1, txOutRef2, ...]
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
    txOpts = def {txOptBalancingPolicy = BalanceWith (wallet 2)}
    ...
  }
```

### Do not automatically balance

```haskell
txSkelTemplate
  { ...
    txOpts = def {txOptBalancingPolicy = DoNotBalance}
    ...
  }
```

## Collaterals

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
    txSkelSignatories = [TxSkelSignatory user1 ... , TxSkelSignatory user2 ... ],
	txSkelCollateralUtxos = CollateralUtxosFromUser user2
    ...
  }
```

* From a direct UTxO list (make sure the owner of these utxo sign the transaction):
```
txSkelTemplate
  { ...
	txSkelCollateralUtxos = CollateralUtxosFromSet (Set.fromList [txOutRef1, txOutRef2])
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
          { txSkelProposalAddress = walletAddress (wallet 1),
            txSkelProposalAction =
              TxGovActionTreasuryWithdrawals $
                Map.fromList
                  [ (toCredential $ wallet 1, Api.Lovelace 100),
                    (toCredential $ wallet 2, Api.Lovelace 10_000)
                  ],
            txSkelProposalWitness = Just (myConstitutionScript, myTxSkelRedeemer),
            txSkelProposalAnchor = Nothing,
			txSkelProposalAutoConstitution = False
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
      [ simpleTxSkelProposal
          (wallet 1)
          (TxGovActionParameterChange [FeePerByte 100, FeeFixed 1_000])
          `withWitness` (myScript, myTxSkelRedeemer)
          `withAnchor` "https://www.tweag.io/"
      ]
    ...
  } 
```

* Enable auto filling of the current offical constitution script.

```haskell 
   txSkelProposalAutoConstitution = True
```

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
	      [ TxSkelWithdrawal myWithdraingPeer (Just 2_000_000),
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

# Transaction modifications

## Tweaks: modify transactions

### Apply a modification

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    bar `withTweak` modification
```

### Add or remove inputs and outputs

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    bar `withTweak` ( do
                        addOutputTweak $ bazValidator `receives` bazPayment
                        removeOutputTweak (\(Pays out) -> somePredicate out)
                        addInputTweak somePkTxOutRef txSkelEmptyRedeemer
                        removeInputTweak (\txOutRef redeemer -> somePredicate txOutRef redeemer)
                    )
```

### Modify signers

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    bar `withTweak` ( do
                        addSignatoriesTweak [signatory1, signatory2]
                        replaceFirstSigner signatory3
                        removeSigners [signatory2]
                    )
```

### Modify skeleton (inputs, outputs, options, etc.) using lenses

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    bar `withTweak` ( do
                        C.overTweak
                          (txSkelOutsL % ix 1 % txSkelOutValueL) -- Value of first output
                          (<> assetClassValue bazAssetClass 10) -- Add 10 baz tokens
                    )
```
