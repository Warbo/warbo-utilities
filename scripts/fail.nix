{ bash, fail, wrap }:

wrap {
  name   = "wrap";
  paths  = [ bash fail ];
  script = ''
    #!/usr/bin/env bash
    exec fail "$@"
  '';
}
