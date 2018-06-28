{ bash, nix-helpers, wrap }:

wrap {
  name   = "wrap";
  paths  = [ bash nix-helpers.fail ];
  script = ''
    #!/usr/bin/env bash
    exec fail "$@"
  '';
}
