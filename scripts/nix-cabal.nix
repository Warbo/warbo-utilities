{ bash, cabal-install, cabal2nix, wrap }:

wrap {
  name   = "nix-cabal";
  paths  = [ bash cabal-install cabal2nix ];
  script = ''
    #!/usr/bin/env bash
    exec nix-shell -E "$(cabal2nix --shell ./.)" --run "cabal $*"
  '';
}
