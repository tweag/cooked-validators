{ 
  sources ? import ./nix/sources.nix {},
  rawpkgs ? import sources.nixpkgs {},
  haskellNix ? import sources.haskellNix {},
  iohkpkgs ? import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs
}:
let
  my-ghcide = (iohkpkgs.haskell-nix.hackage-package {
    compiler-nix-name = "ghc884";
    name = "ghcide";
    flag = "ghc-patched-unboxed-bytecode";
    version = "1.5.1";
  }).components.exes.ghcide;
in rawpkgs.mkShell {
    buildInputs = with rawpkgs; [
        # libs
        libsodium
        lzma
        zlib

        # required to build in a pure nix shell
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal
        systemd.dev
     ] ++ [
        iohkpkgs.haskell-nix.internal-cabal-install
        iohkpkgs.haskell-nix.compiler.ghc810420210212
        my-ghcide
        (iohkpkgs.haskell-nix.tool "ghc810420210212" "haskell-language-server" "latest")
     ];
}
