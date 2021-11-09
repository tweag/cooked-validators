### Updating Plutus

We are _not_ pinning plutus with nix in favor of a simpler nix setup and
to be able to cache CI builds more easily. This does make the update process
more manual but its also pretty straightforward.

Our `cabal.project` file is a copy of the homonym file from 
[plutus-apps](https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project),
with the addition of `plutus-apps` themselves in there and a different list
of packages to build.

In order to bump plutus, all one has to do is:

1. Select the tag you want to updat to from `plutus-apps`.
2. Copy the `cabal.project` from there
3. Modify the `packages:` section to build our packages
4. Add the `plutus-apps` dependency in there:
    ```
    source-repository-package
      type: git
      location: https://github.com/input-output-hk/plutus-apps.git
      tag: v2021-11-05
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
5. Run `cabal build all` and grab a coffee.

### Pre-commit Hooks and CI

Our ci runs `ormolu`, `hlint` and `cabal test` for each of our subprojects.
In order to help avoid CI failures due to formatting problems, we recommend
that you install the [pre-commit hook for running ormolu](tests/ormolu-pre-commit-hook.sh).
To do so, simply copy (or link) the script into `.git/hooks/pre-commit`.

### Nix

A Nix shell development environment is provided.
See [`nix-shell` docs](https://nixos.org/manual/nix/unstable/command-ref/nix-shell.html).

#### Seamless Integration with `direnv`

Install `direnv` and run `direnv allow` at the root of the repo. 
To cache the nix environment and make direnv load faster you can configure
and use [nix-direnv](https://github.com/nix-community/nix-direnv#with-nix-env).

#### Nixpkgs pin

In order to improve reproducibility, nixpkgs is pinned.
See ["FAQ/Pinning Nixpkgs" wiki](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)

Pinning is done using [`niv`](https://github.com/nmattia/niv).

##### Update

```
nix-shell -p niv --run "niv update nixpkgs"
```

Check out the `nix/sources.json` file, you might need to switch the branch.
