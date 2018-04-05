{ haskellPackages, pkgconfig, wrap }:

with builtins;
wrap {
  name   = "hsConfig";
  paths  = [ haskellPackages.cabal-install pkgconfig ];
  script = ''
    #!/usr/bin/env bash
    set -e
    [[ -d .cabal-sandbox ]] || cabal sandbox init
    cabal new-configure -v --enable-tests --enable-benchmarks "$@"
  '';
}
