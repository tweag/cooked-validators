{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc8107;

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            ormolu.enable = true;

            ## FIXME: The upstream `hpack` hook is completely broken, so we
            ## write our own, heavily inspired by theirs but introducing some
            ## fixes. The bugs have been reported at
            ##
            ## https://github.com/cachix/pre-commit-hooks.nix/issues/235
            ##
            ## and we should simply update pre-commit-hooks, remove all this and
            ## replace it by `hpack.enable = true` once they are fixed.
            hpack-fixed = {
              enable = true;
              entry = let
                hpack-dir = pkgs.writeShellApplication {
                  name = "hpack-dir";
                  text = ''
                    set -e
                    find . -type f -name package.yaml | while read -r file; do
                        ${pkgs.hpack}/bin/hpack --force "$file"
                    done
                  '';
                };
              in "${hpack-dir}/bin/hpack-dir";
              files = "(\\.l?hs(-boot)?$)|(\\.cabal$)|((^|/)package\\.yaml$)";
              pass_filenames = false;
            };
          };
        };
      in {
        formatter = pkgs.nixfmt;

        devShells = let
          ## The minimal dependency set to build the project with `cabal`.
          buildInputs = ([ hpkgs.ghc ]) ++ (with pkgs; [
            cabal-install
            libsodium
            secp256k1
            pkg-config
            zlib
            xz
            z3
            postgresql # For pg_config
          ]);

          ## Needed by `pirouette-plutusir` and `cooked`
          LD_LIBRARY_PATH = with pkgs;
            lib.strings.makeLibraryPath [ libsodium zlib xz z3 ];
        in {
          ci = pkgs.mkShell {
            inherit buildInputs;
            inherit LD_LIBRARY_PATH;
          };

          default = pkgs.mkShell {
            ## NOTE: `pkgs.ormolu` must appear before `hpkgs.haskell-language-server`
            ## in the `buildInputs`, so as to take precedence. This ensures that the
            ## version of Ormolu available in the path is that of nixpkgs and not the
            ## one pinned by HLS.
            buildInputs = buildInputs ++ (with pkgs; [ ormolu hpack hlint ])
              ++ (with hpkgs; [ haskell-language-server ]);
            inherit (pre-commit) shellHook;
            inherit LD_LIBRARY_PATH;
          };
        };

        checks = { inherit pre-commit; };
      });

  nixConfig = {
    extra-trusted-substituters = [ "https://tweag-plutus-libs.cachix.org/" ];
    extra-trusted-public-keys = [
      "tweag-plutus-libs.cachix.org-1:0BeVJYx8DnUWJWapRDZeLPOOboBUy3UwhvONd5Qm2Xc="
    ];
  };
}
