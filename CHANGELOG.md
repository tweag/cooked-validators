# Changelog

## [Unreleased]

### Added

- Modifiers to ease specification of payments in transaction skeletons:
  - `withDatum`, `withInlineDatum`, and `withDatumHash` to add or override
    datums in payments, regardless of whether the type matches the validator
    type in case of scripts
  - `paysScriptNoDatum` to be used with `withDatum`, `withInlineDatum` and
    `withDatumHash`.
  - `withReferenceScript` and `withStakingCredential` to add a reference script
    or staking credential to a payment
- Export `Cooked.UtxoState`
- A module `Cooked.ShowBS` to provide a Plutus-level analogue of `Show` with
  `BuiltinString` as its codomain. This is very inefficient due to limitations
  of `BuiltinString`, but potentially useful for "printf-debugging" of scripts.
- An option `txOptEmulatorParamsModification` to temporarily change protocol
  parameters for balancing and validation of a transaction
- A function `combineModsTweak` to construct branching tweaks depending on the
  different combinations of foci of an optic on `TxSkel`
- New `PrettyCooked` instances for common Plutus types
- Tweaks on signers in the non-lens tweak API
- A function `resolveTypedDatum` to recover typed data on UTxOs in
  `MonadBlockChainBalancing`.
- A `UtxoSearch` that starts from a list of `TxOutRef`s
- A transaction option to choose which UTxOs can be spent for balancing
- Lenses for the fields of `TxOpts`
- [Cheatsheet](doc/CHEATSHEET.md)
- API now exposes: `Cooked.Tweak.ValidityRange`, `interpretAndRun`, 
  `interpretAndRunWith`, `runTweak`, `runTweakFrom` and `datumHijackingTarget`

### Removed

- `paysPKWithReferenceScript` (superseded by the `withReferenceScript`
  modifier)
- Do not export `Cooked.UtxoPayloadSet`

### Changed

- `Cooked.holdingInState` is relpaced by `Cooked.holdsInState` which takes an
  address instead of a wallet as argument.
- Failure testing is slightly modified so that every test has to check that the
  right error is thrown
  * `Cooked.testFailsFrom'` is renamed to `Cooked.testFailsFrom`
  * `Cooked.testFails` is (the new) `Cooked.testFailsFrom` with the default
    distribution.

  To update their code, users must
  1. Adapt invokations of `Cooked.testFails` and `Cooked.testFailsFrom` adding
     a predicate that must hold on the error returned by running the
     transaction,
  2. Rename `Cooked.testFailsFrom'` into `Cooked.testFailsFrom`.
  3. (Bonus) simplify, knowing that ``Cooked.testFailsFrom o x def ==
     Cooked.testFails o x``

### Fixes

- Add forgotten export of `permanentValue`
- In `MockChainT`: don't delete data on transaction inputs if there are still
  UTxOs with that datum around. (See PR #354)
- Prettyprint unresolved transaction inputs

## [[2.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v2.0.0) - 2023-02-28

This major update overhauls the entire library to: handle Plutus V2 features,
improve transaction generation, the API, and the internal module structure and
code quality.

### New features

- Reference inputs can be declared in transaction skeletons.
- Reference scripts can be declared in outputs of transaction skeletons and one
  can spend inputs from a script that a transaction references.
- Datums in outputs of transaction skeletons can be declared as
   - inlined,
   - hashed, with the resolved datum included on the transaction (i.e. as in
     Plutus V1), or
   - hashed, without the resolved datum on the transaction.
- New framework to search for UTxOs in the state using chainable filters that
  bring more type information.
- Parameterizable and revamped pretty-printing relying on `prettyprinter`

### Changes

- Transaction skeletons are now defined declaratively, no longer using lists of
  constraints.
- Balancing and transaction generation no longer rely on `plutus-apps`, they
  are performed entirely by cooked.
- Transaction skeletons need an explicit signer (no longer signed by a default
  wallet).
- Modules have been reorganized in a flatter tree and cleaned up.

## [[1.0.1]](https://github.com/tweag/cooked-validators/releases/tag/v1.0.1) - 2023-02-17

### Fixes

- Fixes wrong version number in the `.cabal` files

## [[1.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v1.0.0) - 2023-01-04

Stable release covering Plutus V1.
