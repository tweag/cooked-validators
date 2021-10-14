{ pkgs? import <nixpkgs> {} }:
let
  inherit (pkgs) stdenv lib;

  # Bring in system deps necessary for building the dependencies
  # of plutus.
  twa-dependencies = stdenv.mkDerivation {
    name = "twa-dependencies";
    propagatedBuildInputs = with pkgs; [ libsodium lzma zlib ];
    unpackPhase = "true";
    installPhase = ''
      export LD_LIBRARY_PATH=${lib.makeLibraryPath [ pkgs.libsodium pkgs.lzma pkgs.zlib ]}:\$LD_LIBRARY_PATH
    '';
  };
in pkgs.mkShell {
    buildInputs = with pkgs; [
        libsodium
        lzma
        zlib
        glibc

        haskell.compiler.ghc8104
        haskellPackages.cabal-install
        haskell-language-server
        stack
        hlint
        ormolu
    ];
    shellHook = ''
      export LD_LIBRARY_PATH=${lib.makeLibraryPath [ pkgs.libsodium pkgs.lzma pkgs.zlib pkgs.glibc ]}:\$LD_LIBRARY_PATH
    '';
}

