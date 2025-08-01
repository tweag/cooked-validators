# Optics in `cooked-validators`

## The `optics-core` library

`cooked-validators` relies on optics to inspect and modify inner parts of
various data types. In particular, `TxSkel` is fully equipped with various
optics, which is particularly useful when paired with `Tweak`s. While some other
libraries revolving around Cardano tend to use optics libraries such as `lens`
or `microlens`, we use
[`optics-core`](https://hackage.haskell.org/package/optics-core) instead. We
believe this is the most advanced library of optics and the most suited to
`cooked-validators`. Its type-level constraints allow optics to be combined in a
sensible and optimal manner, which we use extensively.

## Optics

Optics are meant to inspect, build, and update structures at arbitrary depth,
without the burden of manually deconstructing said structures, and without even
the need of knowing the shape of the structure itself. `cooked-validators` uses
optics to inspect transaction skeletons, both to retrieve simple elements and to
inspect the types of more complex ones. For instance, you will find optics to
retrieve or set a typed datum from an output, to set a specific transaction
skeleton option, or to conveniently update certain parts of a given
value. Overall, you will find optics to work with everything in
`cooked-validators`. If one happens to be missing, please report it in our
[issue tracker](https://github.com/tweag/cooked-validators/issues).

## Optics kinds, naming convention, example and usage

All our optics names are built the same way: they start with the containing
structure name, then the focused inner part name, and end with the name of the
optics kind.

- Getters are used to retrieve a value from a structure, and end with `G`. An
  example of a getter is `txSkelOutAddressG`, which builds an address from a
  skeleton output. These optics can be used with `view`.
- Lenses are used to set or retrieve an existing part of a structure, and end
  with `L`. An example of a lens is `valueAssetClassAmountL`, which sets or
  retrieves the amount of a certain asset class in a given value. These optics
  can be used with `view`, `over`, and `set`.
- Affine folds are used to retrieve an optional value from a structure, and end
  with `AF`. An example of an affine fold is `txSkelOutValidatorHashAF`, which
  retrieves a possible validator hash owning an output. This might not exist as
  outputs can be owned by public keys as well, which is handled by
  `txSkelOutPKHashAF`. These optics can be used with `preview`.
- Affine traversals are used to retrieve or set an optional value from a
  structure, and end with `AT`. An example of an affine traversal is
  `txSkelRedeemerTypedAT`, which attempts to retrieve a certain typed redeemer
  from a redeemer, and can also set this field, in a type-changing way. These
  optics can be used with `preview`, `over`, `set`, and `matching`.
- Prisms are used to build from or retrieve a certain value from a structure,
  and end with `P`. An example of a prism is `valueLovelaceP`, which either
  retrieves the amount of lovelace from a value or builds a value from this
  amount. These optics can be used with `preview`, `review`, `over`, `set`, and
  `matching`.
- Isos are used to represent isomorphisms, and end with `I`. An example of an
  iso is `lovelaceIntegerI`, which sees an integer as a lovelace and
  vice-versa. Isos can be reversed using `re` in addition to being used with
  `view`, `review`, `over`, and `set`.
- Reviews are used to represent smart constructors, and end with `R`. An example
  of a review is `txSkelProposalSimpleR`, which builds a `TxSkelProposal` from a
  simple governance action and a credential. Reviews are used with `review`.

## Combining optics

Optics can be combined arbitrarily, as long as the parts they focus
match. Combining optics is done using the `%` operator, which gives a resulting
optics with the most capabilities possible. For instance, combining a lens with
a getter will result in a getter, while combining an affine traversal with an
iso will give an affine traversal. In `cooked-validators`, we only provide
first-order optics, that is, optics working on direct components of the data
type at hand, and leave it to the users to combine them as they please. Examples
of optics combination, including the use of more advanced features such as
traversals and folds, can be found in the module `Cooked.Skeleton` in functions
`txSkelWithdrawnValue`, `txSkelWithdrawingScripts`, `txSkelProposingScripts` and
`txSkelMintingScripts`.
