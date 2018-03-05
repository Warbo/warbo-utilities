{ bash, chromium-launcher, wrap }:

wrap {
  name   = "chromium-repl";
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
    "$chromium" --headless --disable-gpu --repl "$@" 2> >(stripCruft >&2) |
      sed -e 's/^>>>//g' | sed -e 's/^ *//g'
  '';
}
