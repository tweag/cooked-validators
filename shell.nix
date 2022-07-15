{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {} }:
let
  ourpkgs = import ./nix/packages.nix {};
in pkgs.mkShell {
    buildInputs = ourpkgs.build-deps ++ ourpkgs.dev-deps;

    # This shell hook was taken from: https://github.com/input-output-hk/ouroboros-network/pull/3649/files
    # and is necessary to set the right locale so tools such as ormolu and graphmod can work
    # with files contaning non-ascii characters.
    shellHook =
      if (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc")
      then ''
        export LANG="en_US.UTF-8"
        export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
        ''
      else ''
        export LANG="en_US.UTF-8"
        '';

    # Ensure that libz.so and liblzma.so are available to TH splices, cabal repl, etc.
    LD_LIBRARY_PATH = ourpkgs.LD_LIBRARY_PATH;
}
