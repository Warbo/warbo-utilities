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
      grep -v '^ALSA lib'                                |
      grep -v '^\['
    }

    export HOME="$FAKEHOME"

    # shellcheck disable=SC2154
    "$chromium" --headless --disable-gpu --mute-audio --repl \
      "$@" 2> >(stripCruft >&2) | sed -e 's/^>>>//g' | sed -e 's/^ *//g'
  '';
}
