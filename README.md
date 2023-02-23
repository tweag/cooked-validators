> **Warning**
>
> This is a development version of the next iteration of `cooked-validators`, with full support of Plutus V2 features. Refer to [version 1.0.0](https://github.com/tweag/cooked-validators/releases/tag/v1.0.0) for the latest stable version, which only supports Plutus V1.

# [Cooked Validators](https://github.com/tweag/cooked-validators/)

Copyright Tweag I/O 2023

With `cooked-validators` you can test Cardano smart contracts by writing
potentially malicious offchain code. You can also use the library to write
"normal" offchain code in a comfortable and flexible way.

In particular, `cooked-validators` helps you
- interact with smart contracts written in Plutus (as well as any other language
  that compiles to UPLC, by loading contracts from byte strings),
- generate and submit transactions declaratively, while automatically taking
  care of missing inputs and outputs, balancing, and minimum-Ada constraints,
- construct sequences of transactions in an easy-to-understand abstraction of
  "the blockchain", which can be instantiated to different actual
  implementations,
- run sequences of transactions in a simulated blockchain,
- apply "tweaks" to transactions right before submitting them, where "tweaks"
  are modifications that are aware of the current state of the simulated
  blockchain, and
- compose and deploy tweaks with flexible idioms inspired by linear temporal
  logic, in order to turn one sequence of transactions into potentially very
  many traces that might be useful test cases.

If you're interested in a one-stop-shop solution to writing Cardano smart
contracts, `cooked-validators` is probably not for you. The library is geared
specifically towards testing and auditing (already existing) on-chain code.

We provide `cooked-validators` as a research prototype under active development,
and it comes _as is_ with no guarantees whatsoever. Check the [license](LICENSE)
for details.

## How to use `cooked-validators`

Most people will want to add a specific commit of `cooked-validators` as a
`source-repository-package` to their `cabal.project`. Have a look at our
[repository of examples](https://github.com/tweag/cooked-smart-contracts) for
how to accomplish such a thing.

## Documentation

The rendered Haddock for the current `main` branch can be found at
[https://tweag.github.io/cooked-validators/](https://tweag.github.io/cooked-validators/)
