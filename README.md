# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2026

`cooked-validators` is a Haskell library for writing reliable, concise, and
expressive off-chain code for Cardano smart contracts, with a primary focus on
testing, auditing, and behavioral exploration.

It allows you to describe transactions at a high level (via what we call
transaction skeletons) and automatically turn them into complete, valid
transactions by handling all mechanical aspects such UTxO selection, balancing,
minimum-Ada constraints, collaterals or fees.

The library is designed to:
- drastically reduce off-chain boilerplate,
- make test scenarios more readable and maintainable,
- facilitate adversarial testing and vulnerability discovery.

Importantly, `cooked-validators` is non-disruptive: everything it automates can
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
- Define modifications aware of the current blockchain state (tweaks), and apply
  them to transactions just before submission.
- Compose and deploy tweaks on sequences of transactions using idioms inspired
  by linear temporal logic.
- Deploy automated attacks on existing transaction sequences, such as datum
  hijacking or double satisfaction attacks, to uncover vulnerabilities.
- Express expected outcomes on the result of running a trace in a precise and
  declarative way, for example by:
    * specifying the expected number of outcomes in case branching occurred,
    * asserting exact error messages in case of failure,
	* ensuring a specific event was triggered during the run,
    * checking that some specific assets are present at a given address in the
      final blockchain state.

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

5. Observe the log of the run, including:
    - The original skeleton, and its balanced counterpart
	- The associated fee and collaterals
	- The final mockchain state, with every wallet's assets (notice the 10 ADA
      payment owned by wallet 2)
	- The value returned by the run (here `()` as we used `validateTxSkel_`)
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

## Blog posts

Several blog posts have been written about `cooked-validators`. As the library
evolves, some code snippets in these posts may have become outdated. However,
the core philosophy remains unchanged, and these articles still provide valuable
insight into how to use the library.

1. [An article](https://www.tweag.io/blog/2023-05-11-audit-smart-contract/)
   explaining how we use `cooked-validators` to conduct smart contract audits.
   
2. [An
   article](https://www.tweag.io/blog/2025-02-20-transaction-generation-automation-with-cooked-validators/)
   describing how transaction skeletons are built in `cooked-validators` and how
   the library constructs complete transactions from them.

3. [An
   article](https://www.tweag.io/blog/2022-01-26-property-based-testing-of-monadic-code/)
   presenting the original idea of using temporal modalities to modify sequences
   of transactions.


4. [An article](https://www.tweag.io/blog/2022-10-14-ltl-attacks/) explaining
   how [linear temporal
   logic](https://en.wikipedia.org/wiki/Linear_temporal_logic) is used in
   `cooked-validators` to deploy modifications over time.

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
