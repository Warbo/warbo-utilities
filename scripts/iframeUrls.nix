{ bash, sleepDumpPage, wrap, xidel }:

wrap {
  name   = "iframe-urls";
  paths  = [ bash xidel ];
  vars   = { inherit sleepDumpPage; };
  script = ''
    #!/usr/bin/env bash
    "$sleepDumpPage" "$@" | xidel -q -e '//iframe/@src' -
  '';
}
