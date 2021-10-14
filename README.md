### Nix

A Nix shell development environment is provided.
See [`nix-shell` docs](https://nixos.org/manual/nix/unstable/command-ref/nix-shell.html).

#### Nixpkgs pin

In order to improve reproducibility, nixpkgs is pinned.
See ["FAQ/Pinning Nixpkgs" wiki](https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs)

Pinning is done using [`niv`](https://github.com/nmattia/niv).

##### Update

```
nix-shell -p niv --run "niv update nixpkgs"
```

Check out the `nix/sources.json` file, you might need to switch the branch.
