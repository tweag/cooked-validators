{ pkgs ? import <nixpkgs> { }, haskellPackages ? pkgs.haskell.packages.ghc8107
, ...
# Allow more arguments for flake-utils, in particular, which seems to call the
# package with a `system` argument
}:
pkgs.mkShell {
  buildInputs = (with haskellPackages; [
    ghc
    cabal-install
    hpack
    haskell-language-server
    hlint
  ]) ++ (with pkgs; [
    libsodium
    pkg-config
    secp256k1
    zlib
    xz
    postgresql # For pg_config
  ]);
}
