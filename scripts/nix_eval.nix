{ bash, hydra, wrap }:

wrap {
  name   = "nix_eval";
  paths  = [ bash hydra ];
  script = ''
    #!/usr/bin/env bash

    PKG_PATH=$(nix-instantiate --eval -E "<nixpkgs>")
    GC="/nix/var/nix/gcroots/per-user/$USER"

    JOB="$PWD/release.nix"
    [[ -z "$1" ]] || JOB=$(readlink -f "$1")

    hydra-eval-jobs "$JOB"   \
        --gc-roots-dir "$GC" \
        -j 1                 \
        --show-trace         \
        -I "pwd=$PWD"        \
        -I "nixpkgs=$PKG_PATH"
  '';
}
