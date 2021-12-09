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
  my-hls = (iohkpkgs.haskell-nix.hackage-package {
    compiler-nix-name = "ghc810420210212";
    name = "haskell-language-server";
    # configureArgs = "--constraint \"ghcide < 1.5\"";
    version = "1.5.1.0";
    modules = [{
          packages.ghcide.patches = [ nix/patches/ghcide_partial_iface.patch ];
          packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
        }];
  }).components.exes.haskell-language-server;
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
        my-hls
     ];
}
