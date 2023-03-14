{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

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
            hpack.enable = true;
          };
          tools = {
            ## This setting specifies which tools to use in the `pre-commit`
            ## hooks. Since we take our tools (`nixfmt`, `ormolu`, `hpack`) from
            ## `nixpkgs`, then we can simply make sure that
            ## `pre-commit-hooks.nix`'s `nixpkgs` input follows ours, so there
            ## is nothing to see here.
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
            glibcLocales
            postgresql # For pg_config
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
            buildInputs = buildInputs ++ (with pkgs; [ ormolu hpack hlint ])
              ++ (with hpkgs; [ haskell-language-server ]);
            inherit (pre-commit) shellHook;
            inherit LD_LIBRARY_PATH;
            inherit LANG;
          };
        };

        checks = { inherit pre-commit; };
      });

  nixConfig = {
    extra-trusted-substituters =
      [ "https://tweag-cooked-validators.cachix.org/" ];
    extra-trusted-public-keys = [
      "tweag-cooked-validators.cachix.org-1:g1TP7YtXjkBGXP/VbSTGBOGONSzdfzYwNJM27bn8pik="
    ];
  };
}
