# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2024

With `cooked-validators` you can test Cardano smart contracts by writing potentially malicious offchain code.
You can also use the library to write "normal" offchain code in a comfortable and flexible way.

In particular, `cooked-validators` helps you:
- interact with smart contracts written in Plutus (as well as any other language
  that compiles to [UPLC](https://plutonomicon.github.io/plutonomicon/uplc),
  like for example [Plutarch](https://github.com/Plutonomicon/plutarch-plutus)
  or [Aiken](https://aiken-lang.org/), by loading contracts from byte strings)
- generate and submit transactions declaratively, while automatically taking
  care of missing inputs and outputs, balancing, and minimum-Ada constraints
- construct sequences of transactions in an easy-to-understand abstraction of
  "the blockchain", which can be instantiated to different actual
  implementations
- run sequences of transactions in a simulated blockchain,
- apply "tweaks" to transactions right before submitting them, where "tweaks"
  are modifications that are aware of the current state of the simulated
  blockchain
- compose and deploy tweaks with flexible idioms inspired by linear temporal
  logic, in order to turn one sequence of transactions into many sequences that
  might be useful test cases, generalized in [Graft](https://github.com/tweag/graft)

The library is geared specifically towards testing and auditing on-chain code.

You are free to copy, modify, and distribute `cooked-validators` under the terms
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

1. If you have no constraint on the version of `plutus-apps`, copy the file
   [`cabal.project`](./cabal.project) to your project and
   [adapt](https://cabal.readthedocs.io/en/stable/cabal-project.html#specifying-the-local-packages)
   the `packages` stanza.
2. Add the following stanza to the file `cabal.project`
   ```cabal.project
   source-repository-package
     type: git
     location: https://github.com/tweag/cooked-validators
     tag: v3.0.0
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
   > import qualified Plutus.Script.Utils.Ada as Pl
   > printCooked . runMockChain . validateTxSkel $
         txSkelTemplate
           { txSkelOuts = [paysPK (walletPKHash $ wallet 2) (Pl.adaValueOf 10)],
             txSkelSigners = [wallet 1]
           }
   [...]
   - UTxO state:
     • pubkey wallet 1
       - Lovelace: 89_828_471
       - (×4) Lovelace: 100_000_000
     • pubkey wallet 2
       - Lovelace: 10_000_000
       - (×5) Lovelace: 100_000_000
     • pubkey wallet 3
       - (×5) Lovelace: 100_000_000
     • pubkey wallet 4
       - (×5) Lovelace: 100_000_000
   [...]
   ```

## Documentation

The rendered Haddock for the current `main` branch can be found at
[https://tweag.github.io/cooked-validators/](https://tweag.github.io/cooked-validators/).

The [CHEATSHEET](doc/CHEATSHEET.md) is a nice entry point and helper to keep on
sight. It contains many code snippets to quickly get an intuition of how to do
things. Use it to discover or search for how to use features of
`cooked-validators`. Note that this is not a tutorial nor a ready-to-use
recipes book.

We also have a [repository](https://github.com/tweag/cooked-smart-contracts) of example contracts with offchain code and tests written using `cooked-validators`.

Please also look at our
[issues](https://github.com/tweag/cooked-validators/issues) for problems that
we're already aware of, and feel free to open new issues!
