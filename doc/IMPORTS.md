# Imports naming convention

## Naming convention

Here is the correspondance between package and prefix for each of our
main dependencies:

### [`cardano-node-emulator`](https://github.com/IntersectMBO/cardano-node-emulator)

- package `plutus-script-utils`, prefix `Script`
- package `plutus-ledger`, prefix `Ledger`
- package `cardano-node-emulator`, prefix `Emulator`

### [`plutus`](https://github.com/IntersectMBO/plutus)

- package `plutus-tx`, prefix `PlutusTx`
- package `plutus-ledger-api`, prefix `Api`

### [`cardano-api`](https://github.com/IntersectMBO/cardano-api)

- package `cardano-api`, prefix `Cardano`

### [`cardano-crypto`](https://github.com/IntersectMBO/cardano-crypto)

- own package, prefix `Crypto`

### [`cardano-ledger`](https://github.com/IntersectMBO/cardano-ledger)

- package `cardano-ledger-shelley`, prefix `Shelley`

## Exception

When using `PlutusTx.Prelude` (from `plutus-tx`) in conjunction with
the `NoImplicitPrelude` language extension, no prefix should be
used. Instead, functions coming from the usual prelude should be
prefixed `Haskell` in those modules instead.
