# Optics in `cooked-validators`

## The `optics-core` library

The library is relying on optics to inspect and modify inner parts of various
datatypes. In particular, `TxSkel` are fully equiped with various optics, which
is particularly useful when paired with `Tweak`s. While some others libraries
revolving around Cardano tend to use optics libraries such as `lens` or
`microlens` we use
[`optics-core`](https://hackage.haskell.org/package/optics-core) instead. We
believe this is the most advanced library of optics and the most suited to
`cooked-validators`. Its type-level constraint allow optics to be combined in a
sensible and optimal manner, which we use extensively.

## Optics

Optics are meant to inspect, build and update structures in an arbitrary depth,
without the burden of manually deconstructing said structures, and without even
the need of knowing the shape of the structure itself. `cooked-validators` uses
optics to inspect transaction skeleton, both to retrieve simplement elements and
to inspect the types of more complex ones. For instance, you will find optics to
retrieve or set a typed datum from an output, to set a specific transaction
skeleton option, or to conveniently update certain parts of a given
value. Overall, you will find optics to work with everything in
`cooked-validators`. If one happens to be missing, please report it in our [issue
tracker](https://github.com/tweag/cooked-validators/issues).

## Optics kinds, naming convention, example and usage

All our optics names are built the same, they start with the containing
structure name, then the focused inner part name, and end with the name of the
optics kind.

- Getters are used to retrieve a value from a struture, and end with `G`. An
  example of a getter is `txSkelOutAddressG` which builds an address from a
  skeleton output. These optics can be used with `view`.
- Lens are used to set or retrieve an existing part of a structure, and end with
  `L`. An example of a lens is `valueAssetClassAmountL` which sets or retrieves
  the amount of a certain asset class in a given value. These optics can be used
  with `view`, `over` and `set`.
- Affine folds are used to retrieve an optional value from a structure, and end
  with `AF`. An example of an affine fold is `txSkelOutValidatorHashAF` which
  retrieves a possible validator hash owning an output. This might not exist as
  outputs can be owned by public keys as well which is handled by
  `txSkelOutPKHashAF`. These optics can be used with `preview`.
- Affine traversals are used to retrieve or set and optional value from a
  structure, and end with `AT`. An example of an affine traversal is
  `txSkelRedeemerTypedAT` which attempts to retrieved a certain typed redeemer
  from a redeemer, and can also set this field, in a type-changing way. These
  optics can be used with `preview`, `over`, `set` and `matching`.
- Prisms are used to build from or retrieve a certain value from a structure,
  and end with `P`. An example of prism is `valueLovelaceP` which either
  retrieves the amount of lovelace from a value, or builds a value from this
  amount. Thse optics can be used with `preview`, `review`, `over`, `set` and
  `matching`.
- Isos are use to represent isomorphisms, and end with `I`. An example of iso is
  `lovelaceIntegerI` which see an integer as a lovelace, and vice-versa. Isos
  can be reverted using `re` in addition to be used with `view`, `review`,
  `over` and `set`.

## Combining optics

Optics can be combined arbitrarily, as long as the part they focus
matches. Combining optics is done using the `%` operator which gives a resulting
optics with the most capabilities possible. For instance, combining a lens with
a getter will result in a getter, while combining an affine traversal with an
iso will give back an affine traversal. In `cooked-validators`, we only provide
first order optics, that is optics working on direct components of the data type
at hand, and leave to the users the task of combining them any way they
please. Examples of optics combination, including using more advanced features
such as traversals and folds, can be found in module `Cooked.Skeleton` in
functions `txSkelWithdrawnValue`, `txSkelWithdrawingScripts`,
`txSkelProposingScripts` and `txSkelMintingScripts`.
