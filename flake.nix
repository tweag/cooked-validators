{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc8107;
      in {
        formatter = pkgs.nixfmt;

        ## `cabal test` is not run here because it requires internet
        ## to fetch the dependencies
        checks.default = pkgs.stdenv.mkDerivation {
          name = "ormolu/hpack checks";
          src = ./.;
          buildInputs = with pkgs; [ coreutils findutils hpack ormolu ];
          buildPhase = ''
            set -o xtrace # See when it fails
            hpack cooked-validators/package.yaml | grep -q 'is up-to-date' || exit 1
            hpack examples/package.yaml | grep -q 'is up-to-date' || exit 1
            hpack pirouette-plutusir/package.yaml | grep -q 'is up-to-date' || exit 1
            ## NOTE: in case of formatting error, check the versions of
            ## ormolu and try replacing by ${pkgs.ormolu}/bin/ormolu
            ## https://discourse.nixos.org/t/nix-shell-buildinputs-ordering-issue/12885/8
            ormolu --mode check $(find . -name '*.hs') || exit 1
          '';
          ## The derivation succeeds if the output is created.
          installPhase = "mkdir -p $out";
        };

        devShells = let
          ## The minimal dependency set to build the project with `cabal`.
          required = ([ haskellPackages.ghc ]) ++ (with pkgs; [
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
          default = pkgs.mkShell {
            buildInputs = required
              ++ (with haskellPackages; [ haskell-language-server ])
              ++ (with pkgs; [ ormolu hpack hlint ]);
            inherit LD_LIBRARY_PATH;
          };

          ci = pkgs.mkShell {
            buildInputs = required;
            inherit LD_LIBRARY_PATH;
          };
        };
      });

  nixConfig = {
    extra-trusted-substituters = [ "https://tweag-plutus-libs.cachix.org/" ];
    extra-trusted-public-keys = [
      "tweag-plutus-libs.cachix.org-1:0BeVJYx8DnUWJWapRDZeLPOOboBUy3UwhvONd5Qm2Xc="
    ];
  };
}
