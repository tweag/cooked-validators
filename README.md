# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2024

`cooked-validators` is a Haskell library to conveniently and efficiently write
off-chain code for Cardano smart contracts. This offchain code will be
specifically geared to testing and auditing the smart contract in question with
further builtin capabilities of the library.

In particular, `cooked-validators` allows the user to:
- interact with smart contracts written in Plutus or any other language that
  compiles to [UPLC](https://plutonomicon.github.io/plutonomicon/uplc), like for
  example [Plutarch](https://github.com/Plutonomicon/plutarch-plutus) or
  [Aiken](https://aiken-lang.org/), by loading contracts from byte strings
- define transactions in a high level, type-retaining data structure
- submit transactions for validation, while automatically taking care of missing
  inputs and outputs, balancing, minimum-Ada constraints, collaterals and fees
- construct sequences of transactions in an easy-to-understand abstraction of
  "the blockchain", which can be instantiated to different actual
  implementations
- run sequences of transactions in a simulated blockchain
- apply "tweaks" to transactions right before submitting them, where "tweaks"
  are modifications that are aware of the current state of the simulated
  blockchain
- compose and deploy tweaks with flexible idioms inspired by linear temporal
  logic, in order to turn one sequence of transactions into many sequences that
  might be useful test cases, generalized in
  [Graft](https://github.com/tweag/graft)
- deploy automated attacks over existing sequences of transactions, such as
  datum hijacking or double satisfaction attacks, in an attempt to uncover
  vulnerabilities

You are free to copy, modify, and distribute `cooked-validators` under the terms
of the MIT license. We provide `cooked-validators` as a research prototype under
active development, and it comes _as is_ with no guarantees whatsoever. Check
the [license](LICENSE) for details.

## How to integrate `cooked-validators` in a project

To use `cooked-validators`, you need
- [GHC](https://www.haskell.org/ghc/download_ghc_9_6_5.html) version 9.6.5
- [Cabal](https://www.haskell.org/cabal) version 3.10 or later

1. `cooked-validators` depends on
[cardano-haskell-packages](https://github.com/input-output-hk/cardano-haskell-packages)
to get cardano-related packages and on
[cardano-node-emulator](https://github.com/IntersectMBO/cardano-node-emulator)
directly. If you have no constraint on the version of this package, copy the
file [`cabal.project`](./cabal.project) to your project and
[adapt](https://cabal.readthedocs.io/en/stable/cabal-project.html#specifying-the-local-packages)
the `packages` stanza.
   
2. Add the following stanza to the file `cabal.project`
   ```cabal.project
   source-repository-package
     type: git
     location: https://github.com/tweag/cooked-validators
     tag: myTag
     subdir:
       .
   ```
   where `myTag` is either a commit hash in the repo, or a tag, such as v4.0.0
   (see [available
   releases](https://github.com/tweag/cooked-validators/releases)).
   
### Example
   
1. Make your project
   [depend](https://cabal.readthedocs.io/en/stable/getting-started.html#adding-dependencies)
   on `cooked-validators` and `plutus-script-utils`
   
3. Enter a Cabal read-eval-print-loop (with `cabal repl`)
   and create and validate a transaction which transfers 10 Ada
   from wallet 1 to wallet 2:
   ```haskell
   > import Cooked
   > import qualified Plutus.Script.Utils.Ada as Script
   > printCooked . runMockChain . validateTxSkel $
         txSkelTemplate
           { txSkelOuts = [paysPK (wallet 2) (Script.adaValueOf 10)],
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

- The rendered Haddock for the current `main` branch can be found
  [here](https://tweag.github.io/cooked-validators/).

- The [CHEATSHEET](doc/CHEATSHEET.md) contains many code snippets to quickly get
an intuition of how to do things. Use it to discover or search for how to use
features of `cooked-validators`. Note that this is not a tutorial nor a
ready-to-use recipes book.

- The [IMPORTS](doc/IMPORTS.md) file describes and helps to understand our
  dependencies and naming conventions for imports.

- We also have a [repository](https://github.com/tweag/cooked-smart-contracts)
of example contracts with offchain code and tests written using
`cooked-validators`. Note that some examples are not maintained and thus written
using older versions of cooked-validators.

- Feel free to visit our [issue
tracker](https://github.com/tweag/cooked-validators/issues) to seek help about
known problems, or report new issues!
