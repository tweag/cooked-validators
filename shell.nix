{ pkgs ? import (import ./nix/sources.nix).nixpkgs {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [
        # libs
        libsodium
        lzma
        zlib
        glibc

        # required to build in a pure nix shell
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal
        systemd.dev

        # build haskell
        haskell.compiler.ghc8104
        haskellPackages.cabal-install

        # devtools
        haskell-language-server
        hlint
        ormolu
    ];
}
