# Imports convention

The Cardano and Plutus ecosystem is enormous. There are dozens of packages
exporting various definitions, and many of them are either deprecated or
outdated. From within those packages, many modules re-export various definitions
from other modules alongside new definitions, which means that two similar
definitions could be imported in different ways. In addition, various
definitions with similar names are actually different based on where they are
defined. Overall, this makes for a very tedious process of importing the right
definitions from the right places, and having a somewhat homogeneous process is
a challenge. This is why this file exists. We detail here the two main keys to
have a standardized way of importing definition in cooked-validators: qualified
modules and preferred import locations.

## Names of qualified modules

Here is the correspondance between package and prefix for each of our main
dependencies:

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
- package `cardano-ledger-conway`, prefix `Conway`

### Exception

When using `PlutusTx.Prelude` (from `plutus-tx`) in conjunction with the
`NoImplicitPrelude` language extension, no prefix should be used. Instead,
functions coming from the usual prelude should be prefixed `Haskell` in those
modules instead.

## Preferred import locations rules

Here is a list of preferred rules to ensure each definition always comes from a
unique location.

### Stick to imports closer to the root definition

When importing definitions that could be found in various modules from the list
above, we always stick to the more close-to-actual-definition module. In
particular, everything that can be imported from `PlutusLedgerApi.V3` (from
plutus-ledger) should. For instance, `Value` should always be coming from
`Api.Value` instead of `Script.Value`.

### Avoid directly importing `Ledger`

`Ledger` is a big module coming from `plutus-ledger` that re-exports many
definitions. It re-exports too many definitions so that it hides where they
really come from, but not enough so that importing `Ledger` alone sufficies in
most projects. Thus, we avoid importing it altogher and instead rely on
`PlutusLedger.V3` and sub-modules `Ledger.*`.
