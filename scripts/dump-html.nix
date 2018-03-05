{ bash, chromium-launcher, wrap }:

wrap {
  name   = "dump-html";
  paths  = [ bash ];
  vars   = { chromium = chromium-launcher; };
  script = ''
    #!/usr/bin/env bash
    FAKEHOME=$(mktemp --tmpdir -d 'dump-html-XXXXX')
    function cleanup {
      rm -rf "$FAKEHOME"
    }
    trap cleanup EXIT

    function stripCruft {
      grep -v 'No protocol specified'                    |
      grep -v 'xcb_connection_has_error() returned true' |
      grep -v '^\['
    }

    export HOME="$FAKEHOME"

    # shellcheck disable=SC2154
    "$chromium" --headless --disable-gpu --dump-dom "$@" 2> >(stripCruft >&2)
  '';
}
