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

        devShells.default = (import ./shell.nix) {
          inherit pkgs;
          inherit haskellPackages;
        };

        ## Same as default without language server &c.
        devShells.ci = pkgs.mkShell {
          buildInputs = (with haskellPackages; [ ghc cabal-install ])
            ++ (with pkgs; [
              libsodium
              pkg-config
              secp256k1
              zlib
              xz
              postgresql # For pg_config
            ]);
          ## Needed by `pirouette-plutusir`
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.libsodium}/lib:${pkgs.zlib}/lib:''${LD_LIBRARY_PATH:+:}"
          '';
        };
      });
}
