# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2026

`cooked-validators` is a Haskell library for writing reliable, concise, and
expressive off-chain code for Cardano smart contracts, with a primary focus on
testing, auditing, and behavioral exploration.

It allows you to describe transactions at a high level (via transaction
skeletons) and automatically turn them into complete, valid transactions by
handling all mechanical aspects such UTxO selection, balancing, minimum-Ada
constraints, collaterals or fees.

The library is designed to:
- drastically reduce off-chain boilerplate,
- make test scenarios more readable and maintainable,
- facilitate adversarial testing and vulnerability discovery.

Importantly, `cooked-validators` is non-invasive: everything it automates can
also be done manually if needed, allowing users to retain full control over
transaction construction when desired.

## Core features

With `cooked-validators`, you can:
- Interact with smart contracts written in Plutus or any language that compiles
to [UPLC](https://plutonomicon.github.io/plutonomicon/uplc), such as
[Plutarch](https://github.com/Plutonomicon/plutarch-plutus) or
[Aiken](https://aiken-lang.org/), by loading contracts from bytestrings.
- Define transactions using a high-level, type-preserving data structure.
- Submit transactions for validation while the library automatically:
    * fills in missing inputs and outputs,
    * performs balancing,
    * enforces minimum-Ada constraints,
    * computes and attaches optimal collaterals and fees,
	* automatically adds script witnesses, including from reference inputs.
- Construct sequences of transactions in a clear, implementation-independent
  abstraction of the blockchain.
- Run transaction sequences in an emulated blockchain.
- Apply tweaks to transactions just before submission, where tweaks are
  modifications aware of the current blockchain state.
- Compose and deploy tweaks on sequences of transactions using idioms inspired
  by linear temporal logic.
- Deploy automated attacks on existing transaction sequences, such as datum
  hijacking or double satisfaction attacks, to uncover vulnerabilities.
- Express expected outcomes of runs in a precise and declarative way, for
  example by:
    * specifying the expected number of outcomes in case branching occurred,
    * asserting exact error messages in case of failure,
	* ensuring a specific event was triggered during the run,
    * checking that some specific assets are present at a given address.

## How to integrate `cooked-validators` in a project

1. `cooked-validators` depends on
[cardano-haskell-packages](https://github.com/input-output-hk/cardano-haskell-packages)
to get cardano-related packages and on
[cardano-node-emulator](https://github.com/tweag/cardano-node-emulator-forked)
directly. If you have no constraint on the version of this package, copy the
file [`cabal.project`](./cabal.project) to your project and
[adapt](https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#specifying-the-local-packages)
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
   where `myTag` is either a commit hash in the repo, or a tag, such as v8.0.0
   (see [available
   releases](https://github.com/tweag/cooked-validators/releases)).

3. Each release of `cooked-validators` is pinned to a specific version of
   [`cardano-api`](https://github.com/IntersectMBO/cardano-api) which in turn
   pins the versions of all other Cardano-related dependencies (including
   Plutus). Make sure your project relies on the same version.

## Example
   
This example shows how to create and validate a simple transaction that
transfers 10 Ada from wallet 1 to wallet 2, without manually handling fees or
balancing.

1. Enter a Cabal read-eval-print-loop (with `cabal repl`)

2. Import your required dependencies
   ``` haskell
   > import Cooked
   > import qualified Plutus.Script.Utils.Value as Script
   ```

3. Define a transaction which transfers 10 Ada from wallet 1 to wallet 2
   ``` haskell
   let myTransaction = txSkelTemplate {txSkelOuts = [wallet 2 `receives` Value (Script.ada 10)], txSkelSignatories = txSkelSignatoriesFromList [wallet 1]}
   ```

4. Send the transaction for validation, and request the printing of the run
   ``` haskell
   printCooked . runMockChain . validateTxSkel_ $ myTransaction 
   ```

5. Observe the log of the run, including all the adjustments made by
   `cooked-validators`, the final mockchain state and returned value.
   ```haskell
   📖 MockChain run log:
	 ⁍ New raw skeleton submitted to the adjustment pipeline:
	   - Validity interval: (-∞ , +∞)
	   - Signatories:
		 - wallet 1 [balancing]
	   - Outputs:
		 - Pays to pubkey wallet 2
		   - Lovelace: 10_000_000
	 ⁍ New adjusted skeleton submitted for validation:
	   - Validity interval: (-∞ , +∞)
	   - Signatories:
		 - wallet 1 [balancing]
	   - Inputs:
		 - Spends #4480b35!3 from pubkey wallet 1
		   - Redeemer ()
		   - Lovelace: 100_000_000
	   - Outputs:
		 - Pays to pubkey wallet 2
		   - Lovelace: 10_000_000
		 - Pays to pubkey wallet 1
		   - Lovelace: 89_828_383
	   - Fee: Lovelace: 171_617
	   - No collateral required
	 ⁍ New transaction successfully validated:
	   - Transaction id: #c095342
	   - Number of new outputs: 2
   ✅ UTxO state:
	 • pubkey wallet 1
	   - Lovelace: 89_828_383
	   - (×3) Lovelace: 100_000_000
	 • pubkey wallet 2
	   - Lovelace: 10_000_000
	   - (×4) Lovelace: 100_000_000
	 • pubkey wallet 3
	   - (×4) Lovelace: 100_000_000
	 • pubkey wallet 4
	   - (×4) Lovelace: 100_000_000
   🟢 Returned value: () 
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

- The [BALANCING](doc/BALANCING.md) file thorougly describes cooked-validator's
  automated balancing mechanism and associated options (including options
  revolving around fees and collaterals).

- The [OPTICS](doc/OPTICS.md) file describes our usage of optics to navigate our
  data structures.

## Additional resources

- We have a [repository](https://github.com/tweag/cooked-smart-contracts) of
  example contracts with offchain code and tests written using
  `cooked-validators`. Note that these examples are not maintained and thus
  written using older versions of the library.

- Feel free to visit our [issue
  tracker](https://github.com/tweag/cooked-validators/issues) to seek help about
  known problems, or report new issues!

- `cooked-validators` is regularly used to audit Cardano smart contracts. You
  can see some of the products with have audited on [this
  page](https://www.tweag.io/audits/) and can get access to a sample of our
  audit reports on [this
  repository](https://github.com/tweag/tweag-audit-reports).

- `cooked-validators` comes with a [template
  repository](https://github.com/tweag/cooked-template) which can be used to
  develop offchain code and/or audit code with the tool.

## License

You are free to copy, modify, and distribute `cooked-validators` under the terms
of the MIT license. We provide `cooked-validators` as a research prototype under
active development, and it comes _as is_ with no guarantees whatsoever. Check
the [license](LICENSE) for details.
