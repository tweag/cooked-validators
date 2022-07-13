{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {} }:
let
  ourpkgs = import ./nix/packages.nix {};
in pkgs.mkShell {
    buildInputs = ourpkgs.build-deps ++ ourpkgs.dev-deps;

    # Ensure that libz.so and liblzma.so are available to TH splices, cabal repl, etc.
    LD_LIBRARY_PATH = ourpkgs.LD_LIBRARY_PATH;
}
