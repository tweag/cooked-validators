{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {} }:
let
  ourpkgs = import ./nix/packages.nix {};
in pkgs.mkShell {
    LANG="C.UTF-8";
    buildInputs = ourpkgs.build-deps ++ ourpkgs.dev-deps;
}
