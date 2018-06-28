{ bash, wrap, writeScript }:

wrap {
  name   = "nix_build";
  paths  = [ bash ];
  vars   = {
    expr = writeScript "nix_build_expr.nix" ''
      with builtins;
      with import <nixpkgs> {};
      with rec {
        src = getEnv "JOB";
        job = if typeOf (import src) == "lambda"
                 then callPackage src {}
                 else import src;
      };
      withDeps (allDrvsIn job) nothing
    '';
  };
  script = ''
    #!/usr/bin/env bash
    JOB="$PWD/release.nix"
    [[ -z "$1" ]] || JOB=$(readlink -f "$1")
    export JOB

    [[ -e "$JOB" ]] || fail "Couldn't find '$JOB'; maybe give an argument?"

    echo "Building derivations from '$JOB'" 1>&2
    nix-build --show-trace --no-out-link -E 'import (builtins.getEnv "expr")'
  '';
}
