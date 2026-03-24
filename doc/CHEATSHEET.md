# Cheatsheet

Welcome to `cooked-validators`' **Cheatsheet** 

You will find here code snippets to help you use the library, which can, and
should, be adapted to your use cases! Learn how to write transactions, fetch
information from the blockchain, submit these transactions as part of a trace,
and run those traces in tests or in a `repl` environement!

While this is not a tutorial, this document should be helpful to new users to
get accustomed to `cooked-validators` as well as old users looking to remember
how things are done in `cooked-validators` !

## Traces 

### Define a trace

A `mockchain` is an emulated blockchain. A trace is a sequence of instructions
in one of our builtin monadic environement representing this `mockchain`:
- `DirectMockChain`: basic capabilities of the mockchain
- `StagedMockChain`: basic capabilities of the mockchain, with the addition of
  branching and temporal modifications. This is the go-to environement !
- `FullMockChain`: all effects available, including low-level effects such as
  builtin-errors, to be used for maximum level of granularity
- `ExtendedStagedMockChain eff`: same as `StagedMockChain` with additional
  custom effects embedded in `eff`, to work in your own dedicated environement !

* In a fixed mockchain:
```haskell
myTrace :: [Direct|Staged|Full]MockChain ()
myTrace = do
  ...
```

* In a mockChain extended with a single effect:
```haskell
myTrace :: ExtendedStagedMockChain FirstEff ()
myTrace = do
  ...
  actionInFirstEff
  ...
```

* In a mockchain extended with several effects (using `Polysemy`'s bundle capability):
```haskell
myTrace :: ExtendedStagedMockChain (Bundle '[FirstEff, SecondEff,..]) ()
myTrace = do
  ...
  sendBundle $ actionInFirstEff
  ...
  sendBundle $ actionInSecondEff
  ...
```

* In a direct set of capabilities:
```haskell
myTrace :: (Members '[MockChainLog, MockChainRead, ...] effs) => Sem effs ()
myTrace = do
  ...
```

### Execute the trace in a `cabal repl`

* With a default mockchain state: 
  `printCooked $ runMockChainDef myTrace`
* With a custom mockchain state: 
  `printCooked $ runMockChain myState myTrace`
* With a custom initial list of payments (or `InitialDistribution`):
  `printCooked $ runMockChainFromInitDist myPaymentsList myTrace`
* With a default configuration (initial state, initial list of payments, and custom function on returned value): 
  `printCooked $ runMockChainFromConf myConf myTrace`

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
  * `testCooked` "myTastyTest" $ myTastyTest
  * `testCookedQC` "myQuickCheckTest" $ myQuickCheckTest
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

## Custom initial distributions of UTxOs

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
* With arbitrary payments (more details on the payments content in the dedicated section)
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

* In a test: ``testCooked "foo" $ mustSucceedTest foo `withInitDist` myInitiDist``
* In a `repl`: `printCooked $ runMockChainFromInitDist myInitDist foo`
* Within a trace: `forceOutputs myInitDist`

## Give human-readable aliases to hashable data (scripts, wallets, ...)

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

## Handling time

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

### Return `TxOutRef`s from transaction outputs from...

* ... the Cardano transaction
  ```haskell
  endpointFoo :: MonadBlockChain m => m (Api.TxOutRef, Api.TxOutRef)
  endpointFoo = do
    cTx <- validateTxSkel $ txSkelTemplate { ..., ... }
    let (txOutRef1, _) : (txOutRef2, _) : _ = utxosFromCardanoTx cTx
    return (txOutRef1, txOutRef2)
  ```
* ... the returned `TxOutRef`s
  ```haskell
  endpointFoo :: MonadBlockChain m => m (Api.TxOutRef, Api.TxOutRef)
  endpointFoo = do
    txOutRef1 : txOutRef2 : _ <- validateTxSkel' $ txSkelTemplate { ..., ... }
    return (txOutRef1, txOutRef2)
  ```

### Resolve all or parts of a `TxOutRef` (get the corresponding `TxSkelOut` elements)

* Get the full `TxSkelOut` from a `TxOutRef`
```haskell
foo :: MonadBlockChain m => Api.TxOutRef -> m ()
foo txOutRef = do
  txSkelOut <- txSkelOutByRef txOutRef
  ...
```

* Get a certain part of a `TxSkelOut` from a `TxOutRef` and an optic
```haskell
foo :: MonadBlockChain m => Api.TxOutRef -> m ()
foo txOutRef = do
  -- A value is always present, use 'viewByRef'
  value <- viewByRef txSkelOutValueL txOutRef
  -- A datum of a given type might not be present, use 'previewByRef'
  Just typedDatum <- previewByRef (txSkelOutDatumL % txSkelOutDatumTypedAT @MyDatumType) txOutRef
  ...
```

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

## Payments

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

### Mint or burn tokens

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

### Min Ada adjustment to an output

* allow min ADA adjustment, by providing a value: ```party `receives` (Value (myToken 5))```
* allow min ADA adjustment, by providing no value: ```party `receives` (Datum myDatum)```
* forbid min ADA adjustment: ```party `receives` (FixedValue $ ada 10) ```

### Use reference inputs in a transaction

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

### Choose which user provides UTxOs to balance a transaction

First signatory (default):

```haskell
txSkelTemplate
  { ...
    txSkelSignatories = [signatory1, signatory2]
    ...
  }
```

Another signatory:

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

## Provide collaterals

From first signer (default):

```
txSkelTemplate
  { ...
     txSkelSignatories = [signatory1, signatory2],
    ...
  }
```

From another wallet:

```
txSkelTemplate
  { ...
    txSkelSignatories = [TxSkelSignatory user1 ... , TxSkelSignatory user2 ... ],
	txSkelCollateralUtxos = CollateralUtxosFromUser user2
    ...
  }
```

From a direct Utxo list (make sure the owner of these utxo sign the transaction):

```
txSkelTemplate
  { ...
	txSkelCollateralUtxos = CollateralUtxosFromSet (Set.fromList [txOutRef1, txOutRef2])
    ...
  }
```

## Search through UTxOs on the ledger

### Fetch all UTxOs on the ledger

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    -- searchResults :: [(Api.TxOutRef, TxSkelOut)]
    searchResults <- runUtxoSearch $ allUtxos
    ...
```

### Fetch all UTxOs belonging to a certain owner

```haskell
foo :: MonadBlockChain m => m () 
foo = do                  
    ...
    -- searchResults, searchResults' :: [(Api.TxOutRef, TxSkelOut)]
    searchResults <- runUtxoSearch $ utxosOwnedBy (wallet 2)
	searchResults' <- runUtxoSearch $ utxosOwnerBy myValidator
    ...
```

### Search for UTxOs satisfying a predicate

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <-
      runUtxoSearch $
        allUtxos `filterWithPred` ((== ada 10) . view txSkelOutValueL)
    ...
```

### Search for UTxOs without datum

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <- runUtxoSearch $ allUtxos `filterWithPureRev` preview (txSkelOutDatumL % txSkelOutDatumContentAT)
    ...
```

### Combine filters in a UTxOs search

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <-
      runUtxoSearch $
        utxosOwnedBy (wallet 2)
		  `filterWithPureRev` preview (txSkelOutDatumL % txSkelOutDatumContentAT)
          `filterWithPred` ((== ada 10) . view txSkelOutValueL)
    ...
```

## Tweaks: modify traces and endpoints

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

```
	myCertificateAction = CommitteResign ...
	myCertificateAction2 = DRepUpdate ...
	...
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

