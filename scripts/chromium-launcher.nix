{ bash, fetchurl, proot, runCommand, scripts, wrap, writeScript }:

wrap {
  name   = "chromium-launcher";
  paths  = [ bash proot ];
  vars   = {
    ce       = scripts.in-chromium-env;
    launcher = wrap {
      name   = "chrome-launcher.sh";
      script = ''
        #!/bin/sh
        chromium --disable-namespace-sandbox --no-sandbox "$@"
      '';
    };
  };
  script = ''
    #!${bash}/bin/bash
    export PATH="/bin:/usr/bin:/sbin:/usr/sbin:$PATH"
    export TMPDIR=/tmp
    export TEMPDIR=/tmp
    export TMP=/tmp
    export TEMP=/tmp

    CHROME_USER_DATA_DIR=$(mktemp --tmpdir -d 'chromium-launcher-XXXXX')
    export CHROME_USER_DATA_DIR
    function cleanup {
      rm -rf "$CHROME_USER_DATA_DIR"
    }
    trap cleanup EXIT

    # shellcheck disable=SC2154
    "$ce" "$launcher" --user-data-dir="$CHROME_USER_DATA_DIR" "$@"
  '';
}
