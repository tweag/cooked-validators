{ 
  sources ? import ./nix/sources.nix {},

  # Bring in our pinned nixpkgs, but also brings in iohk's modiied nixpkgs
  # which contains the precious ghc810420210212 needed for compiling plutus.
  rawpkgs ? import sources.nixpkgs {},
  haskellNix ? import sources.haskellNix {},
  iohkpkgs ? import
    haskellNix.sources.nixpkgs-unstable
    haskellNix.nixpkgsArgs
}:
let
  # The only difficulty we face is making sure to build the haskell-language-server
  # with the same version of GHC that is used for plutus; doing so, however,
  # requires patching ghcide, a dependency of haskell-language-server.
  #
  # To do that, we create a hackage-package and specify a patch
  # inside its 'modules' key:
  custom-hls = (iohkpkgs.haskell-nix.hackage-package {
    compiler-nix-name = "ghc810420210212";
    name = "haskell-language-server";
    version = "1.5.1.0";
    modules = [{
          packages.ghcide.patches = [ nix/patches/ghcide_partial_iface.patch ];
          packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
        }];
  }).components.exes.haskell-language-server;
in rawpkgs.mkShell {
    buildInputs = with rawpkgs; [
        # libs required to build plutus
        libsodium
        lzma
        zlib

        # required to build in a pure nix shell
        git
        cacert # git SSL
        pkg-config # required by libsystemd-journal
        systemd.dev

        # haskell development tools pulled from regular nixpkgs
        hpack
        hlint
        ormolu
     ] ++ [
        # iohk-specific stuff that we require
        iohkpkgs.haskell-nix.internal-cabal-install
        iohkpkgs.haskell-nix.compiler.ghc810420210212
        custom-hls
     ];
}
