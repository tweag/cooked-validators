# Cheatsheet

* This cheastsheet is quick reminder, not a tutorial about `cooked-validators`.
* Minimum prior knowledge (Cardano, general idea of what `cooked-validators` is about) is expected.
* It reminds how to use or help discover `cooked-validators` features.
* Code snippets are not usable as is, they give intuition and direction. Adapt them to your use case.

## Basics

### Run a trace

* In a test
    * `Tasty.testCase "foo" $ C.testSucceeds foo`
    * `Tasty.testCase "foo" $ C.testFails foo`
* In the REPL
    * `printCooked $ interpretAndRun foo` for all traces
    * `printCooked $ runMockChain foo` for `MonadBlockChain` traces only

### Use a custom initial distribution of value

```haskell
initDist :: InitialDistribution
initDist = initialDistribution [(i, [lovelaceValueOf 25_000_000]) | i <- knownWallets]
```
* In a test `Tasty.testCase "foo" $ testSucceedsFrom def initDist foo`
* In the REPL `printCooked $ interpretAndRunWith (runMockChainTFrom initDist) foo`

### Give human-readable names to pubkey/script/minting hashes

```haskell
pcOpts :: C.PrettyCookedOpts
pcOpts =
  def
    { C.pcOptHashes =
        def
          { C.pcOptHashNames =
              defaultHashNames
                <> C.hashNamesFromList
                  [ (alice, "Alice"),
                    (bob, "Bob"),
                    (carrie, "Carie")
                  ]
                <> C.hashNamesFromList
                  [ (nftCurrencySymbol, "NFT"),
                    (customCoinsCurrencySymbol, "Custom Coins")
                  ]
                <> C.hashNamesFromList
                  [ (fooValidator, "Foo")
                  ]
          }
    }
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
    (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentTime
    ...
```

### Wait for at least some amount of time

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    (firstMsOfCurrentSlot, lastMsOfCurrentSlot) <- currentTime
    targetSlot <- getEnclosingSlot $ lastMsOfCurrentSlot + 3_600_000 -- 1 hour
    awaitSlot targetSlot
    ...
```

### Submit a transaction

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    cardanoTx <-
      validateTxSkel $
        txSkelTemplate
            { txSkelIns = ...,
              txSkelOuts = ...,
              txSkelMints = ...,
              txSkelSigners = ...
            }
    ...
```

### Use wallets

* 10 wallets: `wallet 1` to `wallet 10`
* `walletAddress (wallet 3)`
* `walletPKHash (wallet 2)`

### Sign a transaction with a wallet

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelSigners = [wallet 1]
          ...
        }
```

### Pay (transaction output)

* `paysPK (walletPKHash (wallet 3)) (lovelaceValueOf 6_000_000)`
* `paysScript fooTypedValidator FooTypedDatum (lovelaceValueOf 6_000_000)`

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelOuts = [paysPK ..., paysScript ...]
          ...
        }
```

### Spend some UTxOs

* No redeemer: `(txOutRef, TxSkelNoRedeemerForPK)`
* With redeemer:
    * Regular script: `(txOutRef, TxSkelRedeemerForScript typedRedeemer)`
    * Reference script: `(txOutRef, TxSkelRedeemerForReferencedScript txOutRefCarryingReferenceScript typedRedeemer)`

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelIns = Map.fromList [(txOutRef1, ...), (txOutRef2, ...)]
          ...
        }
```

### Get `TxOutRef`s from transaction outputs

```haskell
endpointFoo :: MonadBlockChain m => m (Pl.TxOutRef, Pl.TxOutRef)
endpointFoo = do
    cTx <- validateTxSkel $ txSkelTemplate { ..., ... }
    let (txOutRef1, _) : (txOutRef2, _) : _ = utxosFromCardanoTx cTx
    return (txOutRef1, txOutRef2)
```

### Resolve a `TxOutRef` (get the corresponding `TxOut`)

```haskell
foo :: MonadBlockChain m => Pl.TxOutRef -> m ()
foo txOutRef = do
    Just txOut <- txOutByRef txOutRef
```

### Resolve the address, value, and datum of a `TxOutRef`

```haskell
foo :: MonadBlockChain m => Pl.TxOutRef -> m ()
foo txOutRef = do
    Just address <- outputAddress <$> txOutByRef txOutRef
    Just value <- valueFromTxOutRef txOutRef
    Just datum <- typedDatumFromTxOutRef @typeOfDatum txOutRef
```

### Mint or burn tokens

```haskell
import qualified Plutus.Script.Utils.Scripts as Pl
```

* No redeemer: `(Pl.Versioned fooPolicy Pl.PlutusV2, NoMintsRedeemer, "fooName", 3)`
* With redeemer: `(Pl.Versioned barPolicy Pl.PlutusV2, SomeMintsRedeemer typedRedeemer, "barName", 12)`
* Burn tokens (negative amount): `(Pl.Versioned bazPolicy Pl.PlutusV2, ..., "bazName", -7)`

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelMints = txSkelMintsFromList
            [ (Pl.Versioned fooPolicy Pl.PlutusV2, ..., ..., ...),
              (Pl.Versioned barPolicy Pl.PlutusV2, ..., ..., ...)
            ]
          ...
        }
```

### Automatically provide enough Ada to output UTxOs

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txOpts = def {txOptEnsureMinAda = True}
          ...
        }
```

### Have pre-existing non-Ada tokens that cannot be minted or burnt

* `initialDistribution [(..., ... <> permanentValue "customToken" 1000), ...]`
* `paysPK ... (permanentValue "customToken" 7)`

### Provide a datum in a pubkey transaction output

* ``paysPK ... `withDatum` FooTypedDatum``

### Inline a datum in a transaction output

* ``paysPK ... `withInlineDatum` FooTypedDatum``
* `paysScriptInlineDatum fooTypedValidator FooTypedDatum (lovelaceValueOf 6_000_000)`

### Provide a hashed datum, that is not resolved in the transaction, in a transaction output

* ``paysPK ... `withDatumHash` FooTypedDatum``
* `paysScriptDatumHash fooTypedValidator FooTypedDatum (lovelaceValueOf 6_000_000)`

### Pay a script a datum whose type may not match the validator's

* ``paysScriptNoDatum fooTypedValidator (lovelaceValueOf 6_000_000) `withDatum` FooTypedDatum``
* ``paysScriptNoDatum fooTypedValidator (lovelaceValueOf 6_000_000) `withInlineDatum` FooTypedDatum``
* ``paysScriptNoDatum fooTypedValidator (lovelaceValueOf 6_000_000) `withDatumHash` FooTypedDatum``

### Use reference inputs in a transaction

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelInsReference = Set.fromList [txOutRef1, txOutRef2, ...]
          ...
        }
```

### Include a reference script in a transaction output

* ``paysPK ... `withReferenceScript` fooTypedValidator``
* ``paysScript... ... `withReferenceScript` fooTypedValidator``

### Include a staking credential in a transaction output

* ``paysPK ... `withStakingCredential` ...``
* ``paysScript... ... `withStakingCredential` ...``

### Spend a referenced script output

```haskell
validateTxSkel
  txSkelTemplate
    { ...
      txSkelIns = Map.fromList [(scriptTxOutRefToSpend, TxSkelRedeemerForReferencedScript txOutRefCarryingReferenceScript redeemer), ...],
      ...
    }
```

## Balancing

### Choose which wallet provides UTxOs to balance a transaction

First signer:

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelSigners = [wallet 1, wallet 2]
          ...
        }
```

Another signer:

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelSigners = [wallet 1, wallet 2],
          txOpts = def {txOptBalanceWallet = BalanceWith (wallet 2)}
          ...
        }
```

### Do not balance with UTxOs carrying a datum

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txOpts = def {txOptBalancingUtxos = BalancingUtxosDatumless}
          ...
        }
```

## Search through UTxOs on the ledger

### Fetch all UTxOs on the ledger

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    -- searchResults :: [(Pl.TxOutRef, Pl.TxOut)]
    searchResults <- runUtxoSearch $ allUtxos
    ...
```

### Fetch all UTxOs at an address

```haskell
foo :: MonadBlockChain m => m () 
foo = do                  
    ...
    -- searchResults :: [(Pl.TxOutRef, Pl.TxOut)]
    searchResults <- runUtxoSearch $ utxosAtSearch (walletAddress (wallet 2))
    ...
```

### Search for UTxOs satisfying a predicate

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <-
      runUtxoSearch $
        allUtxos `filterWithPred` ((== Pl.lovelaceValueOf 10_000_000) . outputValue)
    ...
```

### Search for UTxOs without datum

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <- runUtxoSearch $ allUtxos `filterWithPure` isOutputWithoutDatum
    ...
```

### Combine filters in a UTxOs search

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    ...
    searchResults <-
      runUtxoSearch $
        utxosAtSearch (walletAddress (wallet 2))
          `filterWithPure` isOutputWithoutDatum
          `filterWithPred` ((== Pl.lovelaceValueOf 10_000_000) . outputValue)
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
                        addOutputTweak $ paysScript bazValidator bazDatum bazValue
                        removeOutputTweak (\(Pays out) -> somePredicate out)
                        addInputTweak somePkTxOutRef C.TxSkelNoRedeemerForPK
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
                          (txSkelOutsL % _head % txSkelOutValueL) -- Value of first output
                          (<> assetClassValue bazAssetClass 10) -- Add 10 baz tokens
                    )
```
