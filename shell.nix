{ pkgs ? import (import ./nix/sources.nix).nixpkgs {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [
        # libs
        libsodium
        lzma
        zlib
        glibc

        # build env
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal

        # build haskell
        haskell.compiler.ghc8104
        haskellPackages.cabal-install

        # devtools
        haskell-language-server
        hlint
        ormolu
    ];
}
