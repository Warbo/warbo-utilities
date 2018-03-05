{ bash, sleepDumpPage, wrap, xidel }:

wrap {
  name   = "iframe-urls";
  paths  = [ bash xidel ];
  vars   = { inherit sleepDumpPage; };
  script = ''
    #!/usr/bin/env bash
    # shellcheck disable=SC2154
    "$sleepDumpPage" "$@" | xidel -q -e '//iframe/@src' -
  '';
}
