# `pirouette-plutusir`

__WARNING__: Extremely experimental! Will certainly have bugs and change a lot!

This is a prototype definition of a `PIR` frontend to `pirouette`. If you're not familiar with
how to define frontends for pirouette, it is recommended to study [`Language.Pirouette.Example`](https://github.com/tweag/pirouette/blob/41fe7a5836469d1fecde69bf13482d54771926ca/src/Language/Pirouette/Example.hs)
first.

The main difference between the `PIR` and `Example` languages is the existence of a "pirouette prelude"
for `PIR`. The [`LanguagePrelude`](https://github.com/tweag/pirouette/blob/41fe7a5836469d1fecde69bf13482d54771926ca/src/Pirouette/Monad.hs#L213) class
specifies a set of definitions to be loaded by pirouette automatically, [here](https://github.com/tweag/pirouette/blob/41fe7a5836469d1fecde69bf13482d54771926ca/src/Pirouette/Runner.hs#L47).


An example of using Pirouette to load and check an incorrectness triple over a `.pir` file
can be found [here](tests/Language/Pirouette/PlutusIR/SymEvalSpec.hs).
