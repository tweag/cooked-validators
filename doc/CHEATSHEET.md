# Cheatsheet

* This cheastsheet is quick reminder, not a tutorial about `cooked-validators`.
* Minimum prior knowledge (Cardano, general idea of what `cooked-validators` is about) is expected.
* It reminds how to use or help discover `cooked-validators` features.
* This does not go in depth into all features, instead it give an overview of their most basic usage.
* Code snippets are not usable as is, they give intuition and direction. Adapt them to your use case.

## Basics

### Run a trace

* In a test
    * `testCooked "foo" $ mustSucceedTest foo`
    * `testCookedQC "foo" $ mustFailTest foo`
* In the REPL
    * `printCooked $ interpretAndRun foo` for all traces
    * `printCooked $ runMockChain foo` for `MonadBlockChain` traces only

### Custom initial distributions of UTxOs

#### Creation

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
  initDist = InitialDistribution
	[ wallet 3 `receives` (Value $ ada 6)
    , fooTypedValidator `receives` (Value (myToken 6) <&&> InlineDatum fooTypedDatum)
	, wallet 2 `receives` (Value (ada 2) <&&> VisibleHashedDatum fooDatum)
	, wallet 1 `receives` (Value (ada 10) <&&> ReferenceScript fooValidator <&&> StakingCredential cred)
	]
  ```
#### Usage

* In a test ``testCooked "foo" $ mustSucceedTest foo `withInitDist` myInitiDist``
* In the REPL `printCooked $ interpretAndRunWith (runMockChainTFrom initDist) foo`

### Give human-readable names to pubkey/script/minting hashes

* Outside the mockchain, for static names in the pretty options direclty:

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

* Inside the mockchain, for dynamic names (depending on on-chain data, such as `TxOutRef`s):

```haskell
myScript <- define "myScript" $ generateScript txOutRef
```

```haskell
myScript <- defineM "myValidator" $ do
  ...
  return $ generateScript txOutRef
```

### Write a trace or endpoint

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    transactionOrEndpoint1
    transactionOrEndpoint2
    transactionOrEndpoint3
```

### Get the current time

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    slot <- currentSlot
    ...
```


```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentTime
    ...
```

### Wait for at least some amount of time

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentMSRange
    targetSlot <- getEnclosingSlot $ lastMsOfCurrentSlot + 3_600_000 -- 1 hour
    awaitSlot targetSlot
    ...
```

### Submit a transaction for validation and...

* ... get the validated Cardano transaction
  ```haskell
  foo :: MonadBlockChain m => m ()
  foo = do
          ...
		  cardanoTx <-
            validateTxSkel $
			  txSkelTemplate
			    { txSkelIns = ...,
                  ... 
		        }
		  ...
  ```
* ... get the generated `TxOutRef`s
  ```haskell
  foo :: MonadBlockChain m => m ()
  foo = do
          ...
		  txOutRefs <-
            validateTxSkel' $
		  	  txSkelTemplate
			    { txSkelIns = ...,
                  ...
                }
          ...
  ```
* ... ignore any returned value
  ```haskell
  foo :: MonadBlockChain m => m ()
  foo = do
          ...
          validateTxSkel_ $
		    txSkelTemplate
		      { txSkelIns = ...,
                ...
              }
          ...
  ```

### Use wallets

* 10 wallets: `wallet 1` to `wallet 10`
* `import Plutus.Script.Utils.Address qualified as Script`
* `Script.toAddress (wallet 3)`
* `Script.toPubKeyHash (wallet 2)`

### Sign a transaction with one or more wallets

```haskell
txSkelTemplate
    { ...
      txSkelSigners = [wallet 1, ...]
      ...
    }
```

### Pay (transaction output)

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

### Build redeemers

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

### Spend some UTxOs

```haskell
txSkelTemplate
    { ...
      txSkelIns = Map.fromList [
	      (txOutRef1, someTxSkelRedeemer red), 
		  (txOutRef2, emptyTxSkelRedeemer `withReferenceInput` txOutRef),
		  (txOutRef3, someTxSkelRedeemerNoAutoFill red2)
	    ]
      ...
    }
```

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

### Choose which wallet provides UTxOs to balance a transaction

First signer (default):

```haskell
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1, wallet 2]
    ...
  }
```

Another signer:

```haskell
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1, wallet 2],
    txOpts = def {txOptBalancingPolicy = BalanceWith (wallet 2)}
    ...
  }
```

### Do not automatically balance

```haskell
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1, wallet 2],
    txOpts = def {txOptBalancingPolicy = DoNotBalance}
    ...
  }
```

## Provide collaterals

From first signer (default):

```
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1, wallet 2]
    ...
  }
```

From another wallet:

```
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1],
	txSkelCollateralUtxos = CollateralUtxosFromWallet (wallet 2)
    ...
  }
```

From a direct Utxo list:

```
txSkelTemplate
  { ...
    txSkelSigners = [wallet 1],
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
                        addSignersTweak [alice, bob]
                        replaceFirstSigner carol
                        removeSigners [eve]
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

### Anchor resolution policy

* Auto resolution using a given map with resolved page content as bytestrings
  (default behavior)

```haskell 
    txSkelOpts = def 
	  { txOptAnchorResolution = AnchorResolutionLocal $ Map.singleton "https://www.tweag.io/" someByteString
	  }
```

* Auto resolution using web requests (very unsafe, prevents reproducibility)

```haskell 
    txSkelOpts = def 
	  { txOptAnchorResolution = AnchorResolutionHttp
	  }
```	

## Withdrawals

* Register a certain credential as a staker with a certain amount of deposit and reward

```haskell 
do
  ...
  registerStakingCred myWithdrawingScript myReward myDeposit
  ...
```	

* Withdraw the reward

```haskell 
    txSkelTemplate
      { txSkelWithdrawals = scriptWithdrawal myWithdrawingScript myTxSkelRedeemer myReward,
	    ...
      }
```	
