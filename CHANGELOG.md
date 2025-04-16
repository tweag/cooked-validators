# Changelog

## Unreleased

### Added

- Module `Cooked.Pretty.Hashable` has been brought back from
  `Cooked.Conversion.ToHash` since it has no purpose being in
  `plutus-script-utils`.
- It is now possible to use `TxSkelOutNoDatum` when paying to scripts, as
  PlutusV3 allows for it. As a consequence, providing no datum in a `Payable` will
  result in the generation of `TxSkelOutDatum ()` for scripts of version 1 or 2,
  and in the generation of `TxSkelOutNoDatum` for scripts of version 3.
- New `define` primitive in `MonadBlockChainBalancing` which allows to bind an
  alias while defining a variable. This is used by the pretty printer to render
  names that are dynamic, i.e. depend on on-chain data, like utxos.
- The list of `MockChainLogEntry` has been integrated into a `MockChainBook`
  which also stores the aliases exported using `define`.
- The testing framewok now allows to predicate over the number of resulting
  traces using new Test field `testSizeProp` and new helper `withExactSize`.
- Support for multi-purpose scripts post CIP69 and Chang hard fork.
- A test over multi-purpose scripts with Minting + Spending purposes.
- Constructor `Mint` with smart constructors `mint` and `burn` to populate the
  `TxSkelMints` field of our skeleton.
- New helper `addHashNames` to add alias in the pretty cooked options without
  overriding the existing default names.
- `OwnerConstraints` and `ReferenceScriptConstraints` for output to clarify some
  types and allow to reuse those constraints in other parts of the code, such as
  attacks or tweaks.
- New helper `txSkelOutReferenceScript` to retrieve the optional reference script
  from an output.
- New testing helpers `isInWallets` and `isInWallet` to ensure wallets have the
  right amount of certain tokens at the end of a mockchain run. New testing
  helpers `happened` and `didNotHappen` to test for the occurrence of a specific
  log event.

### Removed

- Modules `Validators.hs` and `Currencies.hs`. Their content has been moved to
  `plutus-script-utils` directly with some improvements and adaptations.
- Modules `Cooked.Conversion.***` which have been integrated to
  `plutus-script-utils`. 
- Two helpers from `Skeleton.hs` that were only used once in `MockChain.Direct`:
  `txSkelValidatorsInOutputs`, `txSkelReferenceScripts`
- `walletCredential`, `walletAddress` replaced by `toCredential` and
  `toAddress` from the classes `ToCredential` and `ToAddress`.
- `txOptAutoReferenceScripts` replaced by local option `txSkelRedeemerAutoFill`
  which can be turned on and off in each `TxSkelRedeemer`.

### Changed

- Update cardano-api to version 10.10, post Chang hard fork.
- Datum hijacking attack now branches on each modified outputs.
- Datum hijacking attack has a simpler interface, no longer relying on optics.
- Datum hijacking can now redirect output to any suitable party, the type of can
  be different from the original owner of the utxo.
- The link to explicit typed validators, validators and minting policies has
  been reduced, relying on conversion type classes whenever possible.
- `TxSkelMints` is now built from a list of dedicated `Mint` constructs instead
  of tuples. This allows to use any kind of scripts that can be seen as
  versioned minting policies, such as multipurpose scripts.
- The default way of building `TxSkelMints` now does not pushes for specifying
  multiple redeemers for the same minting policy.
- `validatorFromHash` changed to `scriptFromHash`
- Token names can now be seen as hashables by the pretty printer and thus giving
  a dynamic alias during mockchain runs using `define`.
- Default hash names map in the pretty printer option have been updated.
- The `removeLabelTweak` now fails if the label is absent from the skeleton.
- `TxSkelRedeemer` has been improved: it directly contains a redeemer content
  (no longer needed type `Redeemer`) and it now contains locally an option to
  either automatically assign a reference input or not.
- The CI now ensures the documentation is filled up properly.
- The testing framework has been slightly improved and homogenized.

### Fixed

- A bug where reference inputs given in the withdrawal redeemer would not be put
  in the set of reference inputs of the generated transaction.
- A bug where hashes were not properly displayed by pretty cooked when involving
  leading zeros in the hexadecimal conversion of their digits.
- Size of reference scripts is now taken into account when computing the maximal
  possible fee for a transaction. As a consequence, our dychotomic balancing
  mechanism now iterates within the proper fee bounds for Conway.

## [[5.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v5.0.0) - 2025-03-17

### Added

- `quickCurrencyPolicyV3` and `permanentCurrencyPolicyV3` which should be the
  most commonly used.
- All kinds of scripts can now be used as reference scripts.
- `validateTxSkel_` which validates a skeleton and ignores the output.
- `txSkelMintsFromList'` which only allows one redeemer per minting policy.
- `validatorToTypedValidatorV2`
- `walletPKHashToWallet` that retrives a wallet from a pkh. It used to be
  present but somehow disapeared.
- It is now possible to reference an output which has a hashed datum.
- `txSkelHashedData` the gives all the datum hashes in inputs and reference inputs.
- Partial support for withdrawals in txSkels. The rewarding scripts will be run
  and assets will be transferred. However, these withdrawals are not properly
  constrained yet.
- PrettyCooked option `pcOptPrintLog`, which is a boolean, to turn on or off the log
  display in the pretty printer. The default value is `True`.
- Reference inputs with the right reference scripts are now automatically
  attached to redeemers when such input exists. This can be turned on using
  `txOptAutoReferenceScripts`. If disabled, the helper `withReferenceInput` can
  be used on a redeemer to manually attach a reference input (which does not
  necessarily have to contain the right reference script).
- Capability to test the result of a mockchain run based on the log entries.
- `txOutRefToTxSkelOut` helper to query the mock chain for recreating a
  `TxSkelOut` from a `TxOutRef`. This is very useful when using `Tweaks` that
  need to pay back an existing output with a slight modification.
- A new tweak `modifySpendRedeemersOfTypeTweak` to apply an optional
  modification of all redeemers of a certain type within the skeleton inputs.
- New setters for the various outputs fields.
- The `Payable` structure to properly define, compose, and later pay, payable
  elements with `receive`.
- The `receive` smart constructor for payments that allows to compose payable
  elements (datums, values, staking credential and reference scripts) and
  associate them to a recipient.
- `TxSkelOutValue` which encompasses both a value and whether it can be tampered
  with through min ada adjustment. It comes with the constructors
  `Value` and `FixedValue` from the `Payable` type.

### Removed

- `positivePart` and `negativePart` in `ValueUtils.hs`. Replaced by `Api.split`.
- Redundant logging of errors in mockchain runs.
- Useless minting of non-ADA value in the dummy initial transaction.
- Payment helpers (such as `PaysPK`, `withDatum` ...). Replaced by `receives`.
- `txOptEnsureMinAda`, replaced by a field of `TxSkelOutValue`

### Changed

- GHC bumped to 9.6.6
- Internal representation of redeemers have changed, and are similar for any
  supported script purpose (minting, spending or proposing).
- Redeemers should now be built using one of the two following smart
  constructors: `someTxSkelRedeemer`, `emptyTxSkelRedeemer`
- `mkProposingScript` changed to `mkScript`
- `withDatumHashed` changed to `withUnresolvedDatumHash`
- `paysScriptDatumHashed` changed to `paysScriptUnresolvedDatumHash`
- `txSkelInputData` changed to `txSkelInputDataAsHashes`
- Pretty printing of hashed datum now includes the hash (and not only the
  resolved datum).
- Logging has been reworked: 
  * it is no longer limited to `StagedMockChain` runs
  * it is now a component of `MonadBlockChainBalancing`
  * it can be turned on/off in pretty-printing options
  * it now displays the discarding of utxos during balancing.
  * it now displays when the user specifies useless collateral utxos.
  * it is not visible from outside of `cooked-validators`
- Dependency to cardano-api bumped to 8.46.
- The whole testing API has been revamped
- File `AddInputsAndOutputs.hs` has been split into `Inputs.hs`, `Outputs.hs`
  and `Mint.hs`. File `TamperDatum.hs` has been updated and integrated into
  `Output.hs`.
- File `Skeleton.hs` has been split into sub-files in the `Skeleton` folder.
- Default language extensions and compilation options have been updated.
- Transaction generation now directly lives in `MonadMockChainBalancing`.
- Initial distributions are now handled as a first action in the `MockChain`.

### Fixed

- A bug where the script hashes would not be computed properly for early plutus
  version (V1 and V2).
- A bug where balancing would fail with excessive inputs and not enough min ada
  in the excess.
- Transactions that do not involve script are now properly generated without any
- All kinds of scripts can now be used as reference scripts.
- A bug where scripts being paid to in the initial distribution would not be
  stored in the MockChain.

## [[4.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v4.0.0) - 2024-06-28

### Added
- File [IMPORTS](doc/IMPORTS.md) to specify how modules should be imported and prefixed
- Instaured a standard for naming imports, homogenized all modules accordingly
- Default language extensions in package.yaml
- A new set of tests `Cooked.BasicUsageSpec` to cover basic use cases
- A new validate function `validateTxSkel'` that directly returns a list of utxos
- An actual content to `hie.yaml` (automatically generated by `gen-hie`)
- Support for collaterals in skeleton options, with three options: auto from
  first signer, auto from given wallet, or given set of utxos.
- Top-level comments to all modules
- `currencySymbolFromLanguageAndMP` to get the right Currency symbol based on a
  plutus version and a minting policy
- `setParams` in `MonadBlockChainWithoutValidation` to account for future
  changes of parameters following votes.
- `txOptCollateralUtxos` to control which utxos should be used as collaterals
- Missing `Eq` instance for `MockChainError`
- Full support in `ShowBS` for printing into bytestring the whole transaction
  context within on-chain code
- `validatorToTypedValidator` which does what its name indicates
- Adding support for `PrettyCooked` for `TxLbl`
- A set of modules (in `Conversion`) that each defines a typeclass of elements
  that can be converted to a certain type. For example `ToPubKeyHash` or
  `ToAddress`.
- New utxos searches `vanillaOutputsAtSearch`, `scriptOutputsSearch`,
  `onlyValueOutputsAtSearch` and `referenceScriptOutputsSearch`
- A test file `Cooked/BalancingSpec.hs` that covers the new balancing mechanism
  extensively.
- A new module `Cooked/MockChain/MinAda.hs` to separate min ada computation from
  the balancing mechanism.
- A new documentation file `doc/BALANCING.md` that extensively describes the new
  balancing mechanism and the available options.
- A new skeleton option to manage fees called `FeePolicy`. It makes it possible
  to successfully validate transactions that have not been automatically
  balanced.
- Auto computation of total and return collaterals based on fees and protocol
  parameters now fully handled during balancing and transaction generation.
- Two filters in `Output.hs`, `isScriptOutput` and `isPKOutput`
- A new helper function to get the full output value of a skeleton,
  `txSkelOutputsValue`
- Proposal procedures can now be issued and described in transaction
  skeletons. If they contain parameter changes or treasury withdrawals, a
  witness script can be attached and will be run.
- `TxSkelRedeemer` is now used for all kinds of scripts.
- File [CONWAY](doc/CONWAY.md) to document which Conway features are currently
  supported.
- A new option `txOptAnchorResolution` to decide whether to resolve urls
  locally or on the web (unsafe). The default is to resolve them locally with a
  given map from urls to page content as bytestring.

### Removed
- Extraneous dependencies in package.yaml
- File `Cooked.TestUtils`, its content has been added to `Cooked.MockChain.Testing`
- Support for importing scripts from bytestring in module Cooked.RawUPLC, to be
  added back later on
- Deprecated skeleton option: `txOptAwaitTxConfirmed`
- Deprecated use of `*` instead of `Type`
- Many unused pragmas
- Orphan default instance for `Ledger.Slot`
- `MintsRedeemer` (replaced by `TxSkelRedeemer`)

### Changed
- Default era from Babbage to Conway
- No longer rely on deprecated plutus-apps, but instead
  [cardano-node-emulator](https://github.com/IntersectMBO/cardano-node-emulator)
- From GHC 8.10.4 to 9.6.5 and associated versions of HLS
- Rely mostly on
  [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages?tab=readme-ov-file)
  instead of direct git sources
- Update the cheatsheet to account for various small changes + collaterals
- `ImportQualifiedPost` by default
- `MockChainEnv` is gone, replaced by the new mcstParams field in `MockChainSt`
- The structure of the various steps around transaction validation (fee
  generation, ensuring min ada...)
- Regrouped all important validation steps, including modifications requested in
  skeleton options in the direct implementation of `validateTxSkel`.
- Homogenized and simplified the functions to generate transaction parts from a
  `TxSkel` by using a reader monad over various parameters.
- Fully reworked the balancing process and associated balancing options.
  See in the dedicated [documentation](doc/BALANCING.md).
- Reworked `MockChain` errors related to balancing.

### Fixed
- A bug where the ledger state would not be updated by consumed collaterals
- A curious choice where parameter changes for single transactions would be
  applied several times instead of one
- Various warnings around incomplete pattern matches when selecting utxos for
  balancing, with more suitable algorithms

## [[3.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v3.0.0) - 2024-03-22

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
- `there` modifier to apply a tweak at a precise place in a trace
- New tweaks to change the start or end of the transaction validity range:
  `setValidityStartTweak` and `setValidityEndTweak`
- UTxo searches with predicates over values, including only ada, or not only
  ada: `filterWithValuePred`, `filterWithOnlyAda` and `filterWithNotOnlyAda`
- New pretty-printing options related to hashes in `pcOptHashes` including the
  possibility to assign human readable names to hashes (pubkeys, scripts,
  minting policies)
- Initial distributions of funds can now include arbitrary payments instead of
  only consisting of values belonging to wallets. In particular, we can now
  initially pay to scripts and have utxos with datums and reference scripts. We
  can still create an initial distribution in the old fashion way with
  `distributionFromList` or directly provide a list of payments with
  `InitialDistribution`.
- Dummy pre-existing validators in `Cooked.Validators` to be used for testing
  purposes mainly but also as targets for attacks and tweaks.
- Small QOL helpers (`ada`, `lovelace` and `adaAssetClass`) to create values in
  `Cooked.ValueUtils`.

### Removed

- `paysPKWithReferenceScript` (superseded by the `withReferenceScript` modifier)
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
  1. Adapt invokations of `Cooked.testFails` and `Cooked.testFailsFrom` adding a
     predicate that must hold on the error returned by running the transaction,
  2. Rename `Cooked.testFailsFrom'` into `Cooked.testFailsFrom`.
  3. (Bonus) simplify, knowing that ``Cooked.testFailsFrom o x def ==
     Cooked.testFails o x``
- Quick and permanent value minting policies have been migrated to PlutusV2.
- Default initial distribution only provides 5 UTxOs per wallet instead of 10.

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
- Balancing and transaction generation no longer rely on `plutus-apps`, they are
  performed entirely by cooked.
- Transaction skeletons need an explicit signer (no longer signed by a default
  wallet).
- Modules have been reorganized in a flatter tree and cleaned up.

## [[1.0.1]](https://github.com/tweag/cooked-validators/releases/tag/v1.0.1) - 2023-02-17

### Fixes

- Fixes wrong version number in the `.cabal` files

## [[1.0.0]](https://github.com/tweag/cooked-validators/releases/tag/v1.0.0) - 2023-01-04

Stable release covering Plutus V1.
