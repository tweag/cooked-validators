# Tweag Plutus Libraries

_These libraries are a research prototype under active development_

* [Projects](#projects)
* [Developer Tools and Env](#developer-tools-and-environment)
  - [IMPORTANT: Nix Cache](#nix)
  - [Updating Plutus](#updating-plutus)

This repository has a collection of the libraries we use for developping and auditing [Plutus](https://github.com/input-output-hk/plutus) contracts.

## Projects

This repository hosts many projects. You are likely only interested in [cooked-validators](cooked-validators) and the [examples](examples) provided therein.

### [cooked-validators](cooked-validators)

- Property-based test Plutus contracts with no code duplication.
- Interacts seamlessly with Plutus `Contract` monad.
- Supports loading UPLC contracts from bytestrings for testing.

### [examples](examples)

- Example contracts and their test suites written using `cooked-validators`

### [tasty-twaudit-metadata](tasty-twaudit-metadata)

- Work-in-progress of internal library that we use to generate LaTeX reports.

## Developer Tools and Environment

All of the project dependencies, except for Plutus, are handled by nix. Plutus is handled by cabal.

### Nix

A Nix shell development environment is provided.
See [`nix-shell` docs](https://nixos.org/manual/nix/unstable/command-ref/nix-shell.html).

#### _IMPORTANT:_ Configure your nix cache!

Plutus uses a custom GHC version. To avoid building GHC, make sure you set up the IOHK binary nix cache
as instructed [here](https://github.com/input-output-hk/plutus#iohk-binary-cache).

#### Seamless Integration with `direnv`

Install `direnv` and run `direnv allow` at the root of the repo.
To cache the nix environment and make direnv load faster you can configure
and use [nix-direnv](https://github.com/nix-community/nix-direnv#with-nix-env).
Several editors have support for `direnv`. If you use emacs, we recomend using [`envrc-mode`](https://github.com/purcell/envrc).

#### Nixpkgs and HaskellNix pin

In order to improve reproducibility, nixpkgs and [`haskell.nix`](https://input-output-hk.github.io/haskell.nix/) are pinned.
See ["FAQ/Pinning Nixpkgs" wiki](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)

Pinning is done using [`niv`](https://github.com/nmattia/niv). Updating them is simple:


```
nix-shell -p niv --run "niv update nixpkgs"
nix-shell -p niv --run "niv update haskellNix"
```

Check out the `nix/sources.json` file, you might need to switch the branch.


### Updating Plutus

We are _not_ pinning plutus with nix in favor of a simpler nix setup and
to be able to cache CI builds easily. This does make the update process
more manual but its also pretty straightforward.

Our `cabal.project` file is a copy of the homonym file from
[plutus-apps](https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project),
with the addition of `plutus-apps` themselves in there and a different list
of packages to build.

In order to bump plutus, all one has to do is:

1. Select the tag you want to update _to_ from `plutus-apps`.
2. Copy the `cabal.project` from there
3. Modify the `packages:` section to build our packages
4. Add the relevant setting for linking with libsodium:
    ```
    package cardano-crypto-praos
      flags: -external-libsodium-vrf
    ```
5. Add the `plutus-apps` dependency in there:
    ```
    source-repository-package
      type: git
      location: https://github.com/input-output-hk/plutus-apps.git
      tag: v2021-11-05 -- change the tag to whatever tag you need.
      subdir:
        freer-extras
        playground-common
        plutus-chain-index
        plutus-chain-index-core
        plutus-contract
        plutus-pab
        plutus-ledger
        plutus-use-cases
        quickcheck-dynamic
    ```
   In this case, we're running with `plutus-apps` at tag `v2021-11-05`.
6. Run `cabal build all` and grab a coffee, this will take a while.

### Pre-commit Hooks and CI

Our ci runs `ormolu`, `hlint` and `cabal test` for each of our subprojects.
In order to help avoid CI failures due to formatting problems, we recommend
that you install the [pre-commit hook for running ormolu](tests/ormolu-pre-commit-hook.sh).
To do so, simply copy (or link) the script into `.git/hooks/pre-commit`.
