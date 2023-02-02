> **Warning**
>
> This is a development version of the next iteration of `cooked-validators`, with full support of Plutus V2 features. Refer to [version 1.0.0](https://github.com/tweag/plutus-libs/releases/tag/v1.0.0) for the latest stable version, which only supports Plutus V1.

# Tweag Plutus Libraries

This repository contains our collection of the libraries for developing and auditing [Plutus](https://github.com/input-output-hk/plutus) contracts.
These libraries are a research prototype under active development, they come
_as is_ with no guarantees whatsoever. Check the [license](LICENSE) for details.

## Documentation

The rendered Haddock for the current `main` branch can be found at [https://tweag.github.io/plutus-libs/](https://tweag.github.io/plutus-libs/)

## Projects

### [cooked-validators](cooked-validators)

- Used for writing the first layer of off-chain code: generating and submitting transactions.
- Enables property-based testing of Plutus contracts with no code duplication.
- Interacts seamlessly with Plutus `Contract` monad.
- Supports loading arbitrary UPLC contracts from bytestrings for testing.

### [examples](examples)

- Example contracts and their test suites written using `cooked-validators`
