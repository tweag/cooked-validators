# Cooked Examples

This is a series of simple contracts that are used to exercise the [`cooked-validators`](../cooked-validators) library.
The best place to start is probably with the Split contract. We took the on-chain part [from the oficial Plutus docs][split-tuto]
but defined the [off-chain](src/Split/OffChain.hs) using `cooked-validators`. Check the [example](../cooked-validators/README.md#a-quick-example)
in the `cooked-validator` README for more detailed information.

[split-tuto]: https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/basic-apps.html#defining-the-validator-script
