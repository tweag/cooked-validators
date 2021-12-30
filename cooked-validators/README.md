# Cooked Validators

* [A Quick Example](#a-quick-example)
* [Guidelines](#guidelines)
* [Using `cooked-validators`](#using-cooked-validators)

Build Cardano transactions and interact with your [Plutus][plutus] validators.
Enables you to write the first layer of your off-chain code as if
you were using the `Contract` monad, but provides property-based tests
at the transaction level.

## A Quick Example

Say you followed the [tutorial on the Split][split-tuto] contract up to and including
the "Defining the validator script" section. At the end,
you should have a [`splitValidator`](../examples/src/Split.hs) function that
executes the _on-chain_ part of the Split contract.

Now, in order to interact with that contract, we need to write the _off-chain_ code
that generates and sends the necessary transactions to the blockchain. We can do
part of that with `cooked-validators`, in a very similar fashion to how we would do it
in the `Contract` monad. Instead of relying on the `Contract` monad directly, though,
we will use an arbitrary monad that is an instance of [`MonadBlockChain`](src/Cooked/MockChain/Monad.hs).

Locking funds is trivial, all we need to do is send the amount of funds we wish to lock
to the script:

```haskell
txLock :: (MonadBlockChain m) => SplitDatum -> m ()
txLock datum = void $ validateTxConstr
      [PaysScript splitValidator [(datum, Pl.lovelaceValueOf $ Split.amount datum)]]
```

Note how `txLock` is defined for an arbitrary `m` such that `MonadBlockChain m`. In particular,
it defined for `Contract` for also for [`MockChain`](src/Cooked/MockChain/Monad/Direct.hs)
and [`StagedMockChain`](src/Cooked/MockChain/Monad/Staged.hs), which is what we will be using for
testing our `splitValidator`.

Unlocking funds is a little more interesting, to keep things simple we will unlock _the first_ split
datum which we're a recipient of.

```haskell
txUnlock :: (MonadBlockChain m) => m ()
txUnlock = do
  pkh <- ownPaymentPubKeyHash
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat splitValidator (isARecipient pkh)
  let half = div amount 2
  void $ validateTxConstr
      [ SpendsScript script () (output, datum),
        PaysPK r1 (Pl.lovelaceValueOf $ half),
        PaysPK r2 (Pl.lovelaceValueOf $ amount - half)
      ]
```

Here, we first get our own pubkey hash, then we select _the first output that belong to `splitValidator`_ such that
we are a recipient. If there is no such output, the pattern-match will fail and we will return an error.
The `isARecipient` predicate is defined as:

```haskell
isARecipient :: Pl.PubKeyHash -> SplitDatum -> a -> Bool
isARecipient pkh datum _ = pkh `elem` [recipient1 datum, recipient2 datum]
```

These two functions are what we could call the core of the _user interface_ of `splitValidator`. In fact, we can use them _as is_ to be our endpoints:

```haskell
endpoints :: (AsContractError e) => Promise w SplitSchema e ()
endpoints = endpoint @"lock" (txLock . mkSplitData)
   `select` endpoint @"unlock" (const txUnlock)
  where mkSplitData :: LockArgs -> SplitDatum
```

And, because `Contract w s e` is an instance of `MonadBlockChain`, thats all there is to it, you can just reuse your `MonadBlockChain` code without any modifications. But we're not interested in _just using it_. We also want
to make sure that our contract implementation is correct.

### Unit Tests

We can start by writing a unit test, that exercises our `splitValidator` and asserts that the execution succeeds:

```haskell
test1 :: TestTree
test1 = testCase "Simple trace succeeds" $ assertSucceeds $ do
  txLock lockParams `as` wallet 1
  txUnlock `as` wallet 2
 where
  lockParams = SplitDatum (walletPKHash $ wallet 2) (walletPKHash $ wallet 3) 2_000_000
```

Here, we're executing `txLock lockParams` from `wallet 1`'s point of view, where we lock 2 ada to be
split amongst wallets 2 and 3, then we attempt to unlock as wallet 2. Our expectation is that this should
possible and hence, we use `assertSucceeds :: StagedMockChain a -> Test.HUnit.Assertion`. Note how we are
using _the same_ `txLock` and `txUnlock`, but instantiated to a different type.

### Property-based Tests

Now that we built some intuition on our system with our unit tests, we can lift them to property-based tests.
Because `GenT m` is also a `MonadBlockChain` for `MonadBlockChain m`, we can rely on any existing quickcheck
machinery to do so. For example,

```haskell
test2 :: TestTree
test2 = testProperty "Arbitrary simple trace succeeds" $ traceSucceeds $ do
  w1  <-  wallet <$> choose (1, 9)
  w2  <- (wallet <$> choose (1, 9)) `suchThat` (/= w1)
  amm <- choose (2_000_000, 4_000_000) -- must be at least minAdaPerUTxO
  let lockParams = SplitDatum (walletPKHash w1) (walletPKHash w2) amm
  txLock lockParams `as` w1
  w' <- oneOf [w1, w2]
  txUnlock `as` w'
```

Which conveniently provides us with a `Tx`-level property-based test to check the correctness
of our `splitValidator`.

### Next Steps

The `cooked-validators` library can do much more than what was shown in this quick introduction,
we invite the interested parties to check the test for our [examples](../examples) contracts for
more.

## Guidelines

When writing functions with `cooked-validators`, the developer should follow some important guidelines
to make sure that the code remains testable. For example, it is strongly advised to _not_ use `guard`
in your `MonadBlockChain` functions. This could prevent a function from _even sending_ a transaction which,
although it might be what you want from a user-interface point of view, we certainly want to send
malformed transactions to study and test how the actual validators react. Instead, split your off-chain
code in (at least) two layers:

```haskell
txAction :: (MonadMockChain m) => ActionParms -> m ()
txAction x = do
  utxos <- findRelevantUTxos
  ... -- compute some relevant datums
  void $ validateTxConstr $
    [ PaysScript ...
    , SpendsPK ...
      ...
    ]

action :: ActionParms -> Contract w s ContractError ()
action x = guard (validParms x) >> txAction x
```

In this case, we can write expressive tests that execute some action even with pottentially malformed
parameters, by using `txAction`. We only check for some internal consistency when defining the actual
code that will be used as part of our user interface.

## Using `cooked-validators`

If you want to _build_ `cooked-validators`, please follow the instructions
to [set up your environment](../README.md#developer-tools-and-environment).

For _using_ `cooked-validators` in your project, you can add the following
to your `cabal.project` file:
```
source-repository-package
 type: git
 location: https://github.com/tweag/plutus-libs
 tag: <whatever-tag-or-commit-you-want>
 subdir:
   cooked-validators
```

[plutus]: https://github.com/input-output-hk/plutus
[split-tuto]: https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/basic-apps.html#defining-the-validator-script
