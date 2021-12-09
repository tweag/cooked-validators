{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {} }:
let
  our-deps = import ./nix/packages.nix {};
in pkgs.mkShell {
    buildInputs = our-deps;
}
