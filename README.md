> **Warning**
>
> This is a development version of the next iteration of `cooked-validators`, with full support of Plutus V2 features. Refer to [version 1.0.0](https://github.com/tweag/cooked-validators/releases/tag/v1.0.0) for the latest stable version, which only supports Plutus V1.

# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2023

With `cooked-validators` you can test Cardano smart contracts by writing
potentially malicious offchain code. You can also use the library to write
"normal" offchain code in a comfortable and flexible way.

In particular, `cooked-validators` helps you
- interact with smart contracts written in Plutus (as well as any other language
  that compiles to [UPLC](https://plutonomicon.github.io/plutonomicon/uplc),
  like for example [Plutarch](https://github.com/Plutonomicon/plutarch-plutus),
  by loading contracts from byte strings),
- generate and submit transactions declaratively, while automatically taking
  care of missing inputs and outputs, balancing, and minimum-Ada constraints,
- construct sequences of transactions in an easy-to-understand abstraction of
  "the blockchain", which can be instantiated to different actual
  implementations,
- run sequences of transactions in a simulated blockchain,
- apply "tweaks" to transactions right before submitting them, where "tweaks"
  are modifications that are aware of the current state of the simulated
  blockchain, and
- compose and deploy tweaks with flexible idioms inspired by linear temporal
  logic, in order to turn one sequence of transactions into many sequences that
  might be useful test cases.

If you're interested in a one-stop-shop solution to writing Cardano smart
contracts, `cooked-validators` is probably not for you. The library is geared
specifically towards testing and auditing (already existing) on-chain code.

You are free to copy, modify, and distribute `cooked-validators` inder the terms
of the MIT license. We provide `cooked-validators` as a research prototype under
active development, and it comes _as is_ with no guarantees whatsoever. Check
the [license](LICENSE) for details.

## How to integrate `cooked-validators` in a project

This guide shows you how to use `cooked-validators` in a haskell project
using [Cabal](https://cabal.readthedocs.io/en/stable/)
to create and validate a simple transaction.

Before using `cooked-validators`, you need
- [GHC](https://www.haskell.org/ghc/download_ghc_8_10_7.html) version 8.10.7
- [Cabal](https://www.haskell.org/cabal)

1. Copy the file [`cabal.project`](./cabal.project) and
   [adapt](https://cabal.readthedocs.io/en/stable/cabal-project.html#specifying-the-local-packages)
   the `packages` stanza to your project
2. Add the following stanza to the file `cabal.project`
   ```cabal.project
   source-repository-package
     type: git
     location: https://github.com/tweag/cooked-validators
     tag: e124a5f2872e180df9d807445c2195f8052bd262
     subdir:
       cooked-validators
   ```
3. Make your project
   [depend](https://cabal.readthedocs.io/en/stable/getting-started.html#adding-dependencies)
   on `cooked-validators` and `plutus-script-utils`
3. Enter a Cabal read-eval-print-loop (with `cabal repl`)
   and create and validate a transaction which transfers 10 Ada
   from wallet 1 to wallet 2:
   ```haskell
   > import Cooked
   > import qualified Plutus.Script.Utils.Ada as Script
   > prettyCooked . runMockChain . validateTxSkel $
         txSkelTemplate
           { txSkelOuts = [paysPK (walletPKHash $ wallet 2) (Script.adaValueOf 10)],
             txSkelSigners = [wallet 1]
           }
   - UTxO state:
     • pubkey #a2c20c7 (wallet 1)
       - Lovelace: 89_828_471
       - (×9) Lovelace: 100_000_000
     • pubkey #80a4f45 (wallet 2)
       - Lovelace: 10_000_000
       - (×10) Lovelace: 100_000_000
     • pubkey #2e0ad60 (wallet 3)
       - (×10) Lovelace: 100_000_000
     • pubkey #557d23c (wallet 4)
       - (×10) Lovelace: 100_000_000
     • pubkey #bf342dd (wallet 5)
       - (×10) Lovelace: 100_000_000
     • pubkey #97add5c (wallet 6)
       - (×10) Lovelace: 100_000_000
     • pubkey #c605888 (wallet 7)
       - (×10) Lovelace: 100_000_000
     • pubkey #8952ed1 (wallet 8)
       - (×10) Lovelace: 100_000_000
     • pubkey #dfe12ac (wallet 9)
       - (×10) Lovelace: 100_000_000
     • pubkey #a96a668 (wallet 10)
       - (×10) Lovelace: 100_000_000
   ```

## Documentation

The rendered Haddock for the current `main` branch can be found at
[https://tweag.github.io/cooked-validators/](https://tweag.github.io/cooked-validators/).

Please also look at our
[issues](https://tweag.github.io/cooked-validators/issues) for problems that
we're already aware of, and feel free to open new issues!
