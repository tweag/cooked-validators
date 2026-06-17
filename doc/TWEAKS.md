# Tweaks in `cooked-validators`

A *tweak* is a state-aware modification of a transaction skeleton (`TxSkel`),
expressed as a `Polysemy` computation with the `Tweak` effect in scope (see
`Cooked.Tweak.Common`). Tweaks are the building blocks of the attack DSL and can
be deployed over time using `Cooked.Ltl`.

This document describes the conventions every tweak in the library follows, so
that their names and behaviors are predictable. If you add a tweak, follow these
rules; if you find a tweak that breaks them, it is a bug.

## Naming

Every tweak is suffixed with `Tweak` (e.g. `addOutputTweak`). The body of the
name is `<verb><Subject>`, optionally followed by a branching marker (see
below). The verb comes from a fixed vocabulary:

| Verb | Meaning |
|------|---------|
| `get` | Read a value out of the skeleton (no modification). |
| `set` | Replace a value wholesale. |
| `modify` | Apply a function to a value. |
| `add` | Insert into a collection. |
| `remove` | Delete from a collection. |
| `ensure` | Make a property hold, doing nothing if it already does. |
| `has` / `is` | Predicate returning `Bool`. |
| `<subject>Satisfies` | Predicate parameterized by a user function. |

### Singular vs. plural

The subject is **singular** when the tweak acts on exactly one element
(`addInputTweak`, `removeSignatoryTweak`) and **plural** when it takes a list or
a predicate ranging over the whole collection (`addSignatoriesTweak`,
`removeInputsTweak`, `removeMintsTweak`). The `Signatories` module is the
reference example of this convention.

## Branching vs. applying to all foci

Many tweaks target an optic with several foci. There are three possible
behaviors, and the name and type must make clear which one applies:

1. **Apply-to-all (deterministic).** The modification is applied to every
   matching focus and a single modified skeleton results. These tweaks do *not*
   require `NonDet`. They carry no branching marker, e.g. `removeInputsTweak`,
   `tamperDatumTweak`.

2. **Branching (non-deterministic multiplicity).** The tweak produces several
   skeletons, typically one per focus or per combination of foci. These tweaks
   require `Polysemy.NonDet` and are marked `...Any` (one focus chosen per
   skeleton) or keep an explicit combinatorial name (`combineModsTweak`,
   `allOutPermutsTweak`, `malformDatumTweak`). The `redirectOutputTweakAll` /
   `redirectOutputTweakAny` pair in `Cooked.Attack.DatumHijacking` illustrates
   the `All` / `Any` contrast.

3. **Fallible (zero-or-one).** The tweak may fail, yielding either the single
   modified skeleton or nothing. It uses `NonDet` (via `guard`) but does *not*
   multiply skeletons. This is *not* considered branching for naming purposes:
   such tweaks carry no `Any` marker (e.g. `addInputTweak`, which fails if the
   input is already present; `intersectValidityRangeTweak`, which fails on an
   empty intersection).

In short: `...Any` in a name always means "this multiplies the number of
skeletons". The presence of `NonDet` alone does not, since it is also used for
plain failure.

## Redeemers

Redeemers occur in five positions of a skeleton: spending (inputs), minting,
proposing, withdrawing, and certifying. Tweaks that modify redeemers are
position-aware and live in `Cooked.Tweak.Redeemers`. The datum-style
`tamper`/`malform` pair has redeemer counterparts (`tamperRedeemerTweak`,
`malformRedeemerTweak`) with the same apply-all/branching semantics as their
datum equivalents.
