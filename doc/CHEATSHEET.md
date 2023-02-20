# Cheatsheet

## Run a trace

* In a test
    * `Tasty.testCase "foo" $ C.testSucceeds foo`
    * `Tasty.testCase "foo" $ C.testFails foo`
* In the REPL
    * `printCooked $ runMockChain foo`

## Use a custom initial distribution of value

```haskell
initDist :: InitialDistribution
initDist = initialDistribution [(i, [lovelaceValueOf 25_000_000]) | i <- knownWallets]
```
* In a test `Tasty.testCase "foo" $ testSucceedsFrom def initDist foo`
* In the REPL `printCooked $ runMockChainFrom initDist foo`

## Write a trace or endpoint

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    transactionOrEndpoint1
    transactionOrEndpoint2
    transactionOrEndpoint3
```

## Get/use the current time

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    t0 <- currentTime
    bar (currentTime + 60_0000)
```

## Submit a transaction

```haskell
foo :: MonadBlockChain m => m ()
foo = do
    validateTxSkel $
        txSkelTemplate
            { txSkelIns = ...,
              txSkelOuts = ...,
              txSkelMints = ...,
              txSkelSigners = ...
            }
```

## Use wallets

* 10 wallets: `wallet 1` to `wallet 10`
* `walletAddress (wallet 3)`
* `walletPKHash (wallet 2)`

## Sign a transaction with a wallet

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelSigners = [wallet 1]
          ...
        }
```

## Pay (transaction output)

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

## Spend some UTxOs

* No redeemer: `(txOutRef, TxSkelNoRedeemerForPK)`
* With redeemer:
    * Regular script: `(txOutRef, TxSkelRedeemerForScript typedRedeemer)`
    * Reference script: `(txOutRef, TxSkelRedeemerForReferencedScript typedRedeemer)`

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txSkelIns = Map.fromList [(txOutRef1, ...), (txOutRef2, ...)]
          ...
        }
```

## Get `TxOutRef`s from transaction outputs

```haskell
endpointFoo :: MonadBlockChain m => m (Pl.TxOutRef, Pl.TxOutRef)
endpointFoo = do
    cTx <- validateTxSkel $ txSkelTemplate { ..., ... }
    let (txOutRef1, _) : (txOutRef2, _) : _ = utxosFromCardanoTx cTx
    return (txOutRef1, txOutRef2)
```

## Mint tokens

```haskell
import qualified Plutus.Script.Utils.Scripts as Pl
```

* No redeemer: `(Pl.Versioned fooPolicy Pl.PlutusV2, NoMintsRedeemer, "fooName", 3)`
* With redeemer: `(Pl.Versioned barPolicy Pl.PlutusV2, SomeMintsRedeemer typedRedeemer, "barName", 12)`

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

## Automatically provide enough Ada to output UTxOs

```haskell
validateTxSkel $
    txSkelTemplate
        { ...
          txOpts = def {txOptEnsureMinAda = True}
          ...
        }
```

## Have pre-existing non-Ada tokens that cannot be minted or burnt

* `initialDistribution [(..., ... <> permanentValue "customToken" 1000), ...]`
* `paysPK ... (permanentValue "customToken" 7)`
