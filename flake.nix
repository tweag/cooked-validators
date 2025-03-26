{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc966;

        ## We change the way 'blst' is built so that it takes into
        ## account the current architecture of the processor. This
        ## is due to a bug where older processors (>= 10 years)
        ## would not be supported. This should not change anything
        ## on newer machines. This could be revised in the future.
        blst-portable = pkgs.blst.overrideAttrs (_: _: {
          buildPhase = ''
            runHook preBuild
            ./build.sh -shared -D__BLST_PORTABLE__ ${
              pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isWindows
              "flavour=mingw64"
            }
            runHook postBuild
          '';
        });

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
          buildInputs = [
            blst-portable
            pkgs.libsodium
            pkgs.secp256k1
            pkgs.pkg-config
            pkgs.zlib
            pkgs.xz
            pkgs.glibcLocales
            pkgs.openssl_3_4
            pkgs.postgresql # For pg_config
            hpkgs.ghc
            hpkgs.cabal-install
          ];

          ## Folders in which to find ".so" files
          LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath [
            blst-portable
            pkgs.libsodium
            pkgs.secp256k1
            pkgs.zlib
            pkgs.xz
            pkgs.openssl_3_4
            pkgs.postgresql # For cardano-node-emulator
            pkgs.openldap # For freer-extras‽
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
            buildInputs = buildInputs ++ [
              pkgs.hpack
              pkgs.hlint
              hpkgs.ormolu
              hpkgs.haskell-language-server
            ];

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
