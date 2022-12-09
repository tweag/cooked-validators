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
          buildInputs = with pkgs; [ findutils hpack ormolu ];
          buildPhase = ''
            hpack cooked-validators/package.yaml | grep -q 'is up-to-date'
            hpack examples/package.yaml | grep -q 'is up-to-date'
            hpack pirouette-plutusir | grep -q 'is up-to-date'
            echo "[OK] Package specifications"
            ormolu --mode check $(find . -name '*.hs') || exit 1
            echo "[OK] Formatting"
          '';
        };

        devShells.default = (import ./shell.nix) {
          inherit pkgs;
          inherit haskellPackages;
        };

        ## Same as default without language server &c.
        devShells.ci = pkgs.mkShell {
          buildInputs = (with haskellPackages; [ ghc cabal-install hpack ])
            ++ (with pkgs; [
              libsodium
              secp256k1
              pkg-config
              zlib
              xz
              postgresql # For pg_config
              bash
            ]);
          ## Needed by `pirouette-plutusir`
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.libsodium}/lib:${pkgs.zlib}/lib:''${LD_LIBRARY_PATH:+:}"
          '';
        };
      });
}
