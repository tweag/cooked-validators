{
  description = "Packaging plutus-libs";

  nixConfig = {
    extra-trusted-substituters = ["https://cache.iog.io" "https://iohk.cachix.org"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="];
    allow-import-from-derivation = "true";
  };

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix/4ee7270856a6ba4617c7e51a6c619f365faad894";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    haskell-nix,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = haskell-nix.legacyPackages.${system};
    in {
      devShells.default = pkgs.mkShell {
        buildInputs =
          (with pkgs; [
            cvc4
            libsodium
            pkg-config
            lzma
            zlib
          ])
          ++ (with pkgs.haskell-nix; [
            cabal-install.ghc810420210212
            compiler.ghc810420210212
          ]);
        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.zlib}/lib/:${pkgs.lzma.out}/lib:${pkgs.libsodium}/lib:''${LD_LIBRARY_PATH:-}"
        '';
      };

      formatter = pkgs.alejandra;
    });
}
