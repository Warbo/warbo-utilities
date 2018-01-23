{ cabal2nix, pkgconfig, wrap }:

with builtins;
wrap {
  name  = "hsConfig";
  paths = [ cabal2nix pkgconfig ];
  vars  = {
    command = "cabal configure -v --enable-tests --enable-benchmarks";
    expr    = ''with import <nixpkgs> {}; { c2n }:
                (tincify (haskellPackages.callPackage c2n {}) {}).env'';
  };
  script = ''
    #!/usr/bin/env bash
    set -e
    C2N=$(cabal2nix ./.)

    # shellcheck disable=SC2154
    nix-shell --show-trace --arg c2n "$C2N" -E "$expr" --run "$command $*"
  '';
}
