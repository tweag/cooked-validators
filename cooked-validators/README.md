# Cooked Validators

* [A Quick Example](#a-quick-example)
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
in the `Contract` monad.

TODO: Finish this quick example section once PR #55 goes in

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
