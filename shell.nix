(import ./default.nix).shellFor {
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
    hpack = "latest";
    ormolu = "latest";
  };
}

# { 
#   sources ? import ./nix/sources.nix {},
#   haskellNix ? import sources.haskellNix {},
#   pkgs ? import
#     # haskell.nix provides access to the nixpkgs pins which are used by our CI,
#     # hence you will be more likely to get cache hits when using these.
#     # But you can also just use your own, e.g. '<nixpkgs>'.
#     haskellNix.sources.nixpkgs-unstable
#     # These arguments passed to nixpkgs, include some patches and also
#     # the haskell.nix functionality itself as an overlay.
#     haskellNix.nixpkgsArgs
# }:
# pkgs.mkShell {
#     buildInputs = with pkgs; [
#         # libs
#         libsodium
#         lzma
#         zlib
# 
#         # required to build in a pure nix shell
#         git
#         cacert # git SSL
#         pkg-config # required by libsystemd-journal
#         systemd.dev
# 
#         # build haskell
#         haskellPackages.cabal-install
# 
#         # devtools
#         haskell-language-server
#         hlint
#         ormolu
#         hpack
#     ];
# }
