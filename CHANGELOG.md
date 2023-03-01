# Changelog

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
