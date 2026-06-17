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

Importantly, `cooked-validators` is non-opinionated: everything it automates can
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

There are two ways for this integration:

1. `cooked-validators`, and all its dependencies, are available on
   [cardano-haskel-packages
   (CHaP)](https://github.com/IntersectMBO/cardano-haskell-packages). To rely
   on a release available there, add the following stanza to your
   `cabal.project`:
   
   ```cabal.project
   repository cardano-haskell-packages
	 url: https://chap.intersectmbo.org/
	 secure: True
	 root-keys:
	   3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
	   443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
	   a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
	   bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
	   c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
	   d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

   index-state:
	 , cardano-haskell-packages 2026-02-13T00:00:02Z
   ```
   
   To find the appropriate index state to fill above, look for
   `cooked-validators` on [CHaP's packages
   list](https://chap.intersectmbo.org/all-packages/).

2. Alternatively, if you want to rely on a specific commit or branch not
   available on CHaP, you can import `cooked-validators` directly from GitHub
   with the following stanza:

   ```cabal.project
   source-repository-package
     type: git
     location: https://github.com/tweag/cooked-validators
     tag: myTag
     subdir:
       .
   ```
   
   where `myTag` is either a commit hash in the repo, or a tag, such as v9.0.0
   (see [available
   releases](https://github.com/tweag/cooked-validators/releases)).
   
   Note that, should you do that, you would likely still need CHaP for all the
   other dependencies.

 Each release of `cooked-validators` is pinned to a specific version of
 [`cardano-api`](https://github.com/IntersectMBO/cardano-api) which in turn pins
 the versions of all other Cardano-related dependencies (including Plutus). Make
 sure your project relies on the same version.

## Example
   
This example shows how to create and validate a simple transaction that
transfers 10 Ada from Alice (wallet 1) to Bob (wallet 2), without manually
handling fees or balancing.

1. Create a new Haskell module, for example `Demo.hs`

2. Import your required dependencies
   ``` haskell
   import Cooked
   import Plutus.Script.Utils.Value qualified as Script
   ```

3. Start the definition of a `MockChain` run:
   ``` haskell
   myDemoRun :: StagedMockChain ()
   myDemoRun = do
   ```
   
4. Define aliases for Alice and Bob:
   ``` haskell
     alice <- define "Alice" $ wallet 1
     bob <- define "Bob" $ wallet 2
   ```
   
5. Give some initial funds to Alice:
   ``` haskell
     forceOutputs_ $ replicate 3 $ alice `receives` Value (Script.ada 10)
   ```
   
5. Take some notes:
   ``` haskell
     noteS "Alice is sending 10 ADA to Bob"
     noteS "I let cooked-validators do the heavy lifting for me"
   ```

6. Submit the transaction:
   ``` haskell
     validateTxSkel_
       txSkelTemplate
         { txSkelOuts = [bob `receives` Value (Script.ada 10)],
           txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
         }
   ```

7. Lookup for the UTxOs now owned by Bob, and assert that he indeed possesses 1:
   ``` haskell 
      bobUtxos <- utxosAt bob
      assert "Bob now has 1 utxo" $ length bobUtxos == 1
   ```

8. Enter a `cabal repl`, run and print the trace:
   ``` haskell
   > printCooked $ runMockChainDef myDemoRun
   ```

5. Observe the output of printing the run, including:
   - The notes you've taken:
	 ```
	 📔 Notes:
	   - Alice is going to send 10 ADA to Bob
	   - I let cooked-validators do the heavy lifting for me
     ```
   - The execution log, including: the original submitted skeleton, 
	 the adjusted skeleton, and the computed fee and collaterals:	 
	 ```
	 📖 MockChain run log:
	   ⁍ New raw skeleton submitted to the adjustment pipeline:
		 - Validity interval: (-∞ , +∞)
		 - Signatories:
		   - Alice [balancing]
		 - Outputs:
		   - Pays to pubkey Bob
			 - Lovelace: 10_000_000
	   ⁍ New adjusted skeleton submitted for validation:
		 - Validity interval: (-∞ , +∞)
		 - Signatories:
		   - Alice [balancing]
		 - Inputs:
		   - Spends #d769532!1 from pubkey Alice
			 - Redeemer ()
			 - Lovelace: 10_000_000
		   - Spends #d769532!2 from pubkey Alice
			 - Redeemer ()
			 - Lovelace: 10_000_000
		 - Outputs:
		   - Pays to pubkey Bob
			 - Lovelace: 10_000_000
		   - Pays to pubkey Alice
			 - Lovelace: 9_826_799
		 - Fee: Lovelace: 173_201
		 - No collateral required
	   ⁍ New transaction successfully validated:
		 - Transaction id: #bff7a56
		 - Number of new outputs: 2
	 ```
   - The evaluation of the assertions:
	 ```
	 ✅ Assertions:
	   - ✔ Bob now has 1 utxo
     ```
   - The final mockchain state:
	 ```
	 💰 UTxO state:
	   • pubkey Alice
		 - Lovelace: 9_826_799
		 - Lovelace: 10_000_000
	   • pubkey Bob
		 - Lovelace: 10_000_000
	 ```
   - The outcome and return value of the run:
	 ```
	 🟢 Success with returned value: ()
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

- The [TWEAKS](doc/TWEAKS.md) file describes the conventions our tweaks (the
  building blocks of the attack DSL) follow, regarding naming and branching
  behavior.

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
