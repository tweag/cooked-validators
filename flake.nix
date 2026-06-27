{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      pre-commit-hooks,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc96;

        ## We change the way 'blst' is built so that it takes into
        ## account the current architecture of the processor. This
        ## is due to a bug where older processors (>= 10 years)
        ## would not be supported. This should not change anything
        ## on newer machines. This could be revised in the future.
        blst-portable = pkgs.blst.overrideAttrs (
          _: _: {
            buildPhase = ''
              runHook preBuild
              ./build.sh -shared -D__BLST_PORTABLE__ ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isWindows "flavour=mingw64"}
              runHook postBuild
            '';
          }
        );

        ## Runs the test suite without coverage, for clean output. 'spec.tix' is
        ## removed first to avoid stale HPC '.tix'/'.mix' mismatches (caused by
        ## '-fignore-hpc-changes' in 'package.yaml'). Extra arguments are
        ## forwarded to the 'tasty' test binary (e.g. '-p PATTERN').
        cooked-test = pkgs.writeShellScriptBin "cooked-test" ''
          rm -f spec.tix
          opts=(--test-option=--color=always)
          for arg in "$@"; do opts+=("--test-option=$arg"); done
          cabal test all "''${opts[@]}"
        '';

        ## Like 'cooked-test' but with coverage analysis (only checked
        ## punctually). The verbose "Writing: ....html" coverage lines are
        ## filtered out, and 'PIPESTATUS' preserves the test binary's exit status
        ## through that pipe.
        cooked-test-coverage = pkgs.writeShellScriptBin "cooked-test-coverage" ''
          rm -f spec.tix
          opts=(--test-option=--color=always)
          for arg in "$@"; do opts+=("--test-option=$arg"); done
          cabal test all --enable-coverage "''${opts[@]}" | grep -vE --color=never "^Writing:.*html$"
          exit "''${PIPESTATUS[0]}"
        '';

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
      in
      {
        formatter = pkgs.nixfmt;

        devShells =
          let
            ## The minimal dependency set to build the project with `cabal`.
            buildInputs = [
              blst-portable
              pkgs.pkg-config
              pkgs.glibcLocales
              pkgs.zlib
              pkgs.libsodium
              pkgs.secp256k1
              pkgs.lmdb
              hpkgs.ghc
              hpkgs.cabal-install
            ];

            ## Folders in which to find ".so" files
            LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath [
              pkgs.xz
              pkgs.zlib
              pkgs.lmdb
              pkgs.openssl_3_6
              pkgs.postgresql # For cardano-node-emulator
              pkgs.openldap # For freer-extras‽
              pkgs.libsodium
              pkgs.secp256k1
              blst-portable
            ];

            LANG = "C.UTF-8";

          in
          {
            ci = pkgs.mkShell {
              inherit buildInputs;
              inherit LD_LIBRARY_PATH;
              inherit LANG;
            };

            default = pkgs.mkShell {
              buildInputs = buildInputs ++ [
                pkgs.hpack
                pkgs.hlint
                pkgs.ormolu
                hpkgs.haskell-language-server
                cooked-test
                cooked-test-coverage
              ];

              inherit LD_LIBRARY_PATH;
              inherit LANG;

              shellHook = pre-commit.shellHook;
            };
          };

        checks = { inherit pre-commit; };
      }
    );

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
