{
  sources ? import ./sources.nix {},

  # Bring in our pinned nixpkgs, but also brings in iohk's modiied nixpkgs
  rawpkgs ? import sources.nixpkgs {},
  haskellNix ? import sources.haskellNix {},
  iohkpkgs ? import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs
}:
{
  # We will split our dependencies into those deps that are needed for
  # building and testing; and those that are needed for development
  # the purpose is to keep CI happier and make it as fast as possible.
  build-deps = with rawpkgs; [
        # libs required to build plutus
        libsodium
        lzma
        zlib

        # required to build in a pure nix shell
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal
        openssl # required by HsOpenSSL required indirectly by cooked-validators

        # We need cvc4 at least to run pirouette
        cvc4

        # haskell development tools pulled from regular nixpkgs
        hpack
        hlint
        ormolu
        cabal-install
        haskell.compiler.ghc8107
     ] ++ [
        # iohk-specific stuff that we require
        iohkpkgs.secp256k1
     ] ++ lib.optional (stdenv.isLinux) systemd.dev;

  # Besides what's needed for building, we also want our instance of the
  # the haskell-language-server
  dev-deps = [
    rawpkgs.haskellPackages.haskell-language-server
  ];

  LD_LIBRARY_PATH = with rawpkgs; lib.strings.makeLibraryPath [zlib lzma];
}
