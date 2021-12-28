# Cooked Validators

* [Getting Started](#getting-started)

Build Cardano transactions and interact with your [Plutus][plutus] validators.
Enables you to write the first layer of your off-chain code as if
you were using the `Contract` monad, but provides property-based tests 
at the transaction level.

## Getting Started

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
