{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {} }:
let
  ourpkgs = import ./nix/packages.nix {};
in pkgs.mkShell {
    buildInputs = ourpkgs.dev-deps;
}
