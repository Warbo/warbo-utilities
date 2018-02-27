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

    export HOME="$FAKEHOME"
    "$chromium" --headless --disable-gpu --dump-dom "$@"
  '';
}
