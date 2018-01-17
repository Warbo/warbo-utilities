{ bash, coreutils, fail, firefox, tightvnc, wrap, writeScript, xdotool, xsel,
  xvfb-run-safe }:
with rec {
  ff = wrap {
    name   = "firefox-runner";
    paths  = [ bash coreutils firefox tightvnc xdotool xsel ];
    script = ''
      #!/usr/bin/env bash
      set -e

      FF_DIR=$(mktemp -d -t 'ff.sh-XXXXX')

      function cleanUp {
        echo "Killing throwaway Firefox instance" 1>&2
        [[ -z "$FF_PID" ]]  ||
          kill -9 "$FF_PID" || echo "He's dead, Jim" 1>&2

        rm -rf "$FF_DIR" || echo "Couldn't delete '$FF_DIR', oh well" 1>&2
      }

      trap cleanUp EXIT

      [[ -n "$TIMEOUT" ]] || TIMEOUT=60

      echo "Opening Firefox on '$URL'" 1>&2
      timeout "$TIMEOUT" firefox -safe-mode         \
                                 -profile "$FF_DIR" \
                                 -no-remote         \
                                 -new-instance      \
                                 "$URL" 1>&2 &
      FF_PID="$!"
      sleep 10

      echo "Skipping safe mode prompt" 1>&2
      xdotool key --clearmodifiers Return
      sleep 15

      [[ -z "$FF_EXTRA_CODE" ]] || "$FF_EXTRA_CODE"

      echo "Opening Web console" 1>&2
      xdotool key ctrl+shift+K
      sleep 10

      echo "Extracting body HTML" 1>&2

      # shellcheck disable=SC2016
      xdotool type 'window.prompt("Copy to clipboard: Ctrl+C, Enter", document.body.innerHTML);'

      sleep 5
      xdotool key --clearmodifiers Return
      sleep 5

      echo "Copying content" 1>&2
      xdotool key ctrl+c
      sleep 5

      echo "Pasting content" 1>&2
      xsel --clipboard
      echo ""

      kill "$FF_PID"
      rm -rf "$FF_DIR"
    '';
  };
};

wrap {
  name   = "ff";
  paths  = [ bash ];
  vars   = {
    inherit ff;
    xvfb     = xvfb-run-safe;
    XVFB_VNC = "1";
  };
  script = ''
    #!/usr/bin/env bash
    URL="$1" "$xvfb" "$ff"
  '';
}
