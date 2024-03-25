{
  ## We pin a specific version because there is a known bug where GHC96X
  ## and HLS 2.6.0.0 do not work together, while nix groups them.
  ## The issue is described here, and a fix is most likely happening soon.
  ## https://github.com/haskell/haskell-language-server/issues/4046
  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/63143ac2c9186be6d9da6035fa22620018c85932";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc963;

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            ormolu.enable = true;
            hpack.enable = true;
          };
          tools = {
            ## This setting specifies which tools to use in the `pre-commit`
            ## hooks. Since we take our tools (`nixfmt`, `ormolu`, `hpack`) from
            ## `nixpkgs`, then we can simply make sure that
            ## `pre-commit-hooks.nix`'s `nixpkgs` input follows ours, so there
            ## is nothing to see here.
            ##
            ## NOTE: Configuring `hpack` here would have no effect. See
            ## https://github.com/cachix/pre-commit-hooks.nix/issues/255
            ## for more information.
          };
        };
      in {
        formatter = pkgs.nixfmt;

        devShells = let
          ## The minimal dependency set to build the project with `cabal`.
          buildInputs = (with hpkgs; [ ghc cabal-install ]) ++ (with pkgs; [
            libsodium
            secp256k1
            pkg-config
            zlib
            xz
            glibcLocales
            postgresql # For pg_config
            blst # required by cardano-node-emulator
          ]);

          ## Needed by `pirouette-plutusir` and `cooked`
          LD_LIBRARY_PATH = with pkgs;
            lib.strings.makeLibraryPath [
              libsodium
              zlib
              xz
              postgresql # For cardano-node-emulator
              openldap # For freer-extras‽
            ];
          LANG = "C.UTF-8";
        in {
          ci = pkgs.mkShell {
            inherit buildInputs;
            inherit LD_LIBRARY_PATH;
            inherit LANG;
          };

          default = pkgs.mkShell {
            ## NOTE: `pkgs.ormolu` must appear before `hpkgs.haskell-language-server`
            ## in the `buildInputs`, so as to take precedence. This ensures that the
            ## version of Ormolu available in the path is that of nixpkgs and not the
            ## one pinned by HLS.
            buildInputs = buildInputs ++ (with pkgs; [ hpack hlint ])
              ++ (with hpkgs; [ ormolu haskell-language-server ]);

            inherit LD_LIBRARY_PATH;
            inherit LANG;

            # In addition to the pre-commit hooks, this redefines a cabal
            # command that gets rid of annoying "Writing: .....*.html" output
            # when running cabal test.
            shellHook = pre-commit.shellHook + ''
              function cabal() {
                    if [ "$1" != "test" ]; then
                      command cabal $@
                    else
                      command cabal --test-option=--color=always $@ | grep -vE --color=never "^Writing:.*html$"
                    fi
              }
              export -f cabal
            '';
          };
        };

        checks = { inherit pre-commit; };
      });

  nixConfig = {
    extra-trusted-substituters = [
      "https://tweag-cooked-validators.cachix.org/"
      "https://pre-commit-hooks.cachix.org/"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "tweag-cooked-validators.cachix.org-1:g1TP7YtXjkBGXP/VbSTGBOGONSzdfzYwNJM27bn8pik="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
