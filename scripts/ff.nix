{ bash, coreutils, dbus, fail, nixpkgs1609, python, wrap, writeScript, xdotool,
  xorg, xsel, xvfb-run-safe }:

with rec {
  inherit (nixpkgs1609) firefox;

  ff = wrap {
    name  = "firefox-runner";
    paths = [ bash coreutils (dbus.tools or dbus) fail firefox xdotool
              xorg.xwininfo xsel ];
    vars  = {
      ffSettings = writeScript "user.js" ''
        // Don't show bookmark icons
        user_pref("browser.shell.checkDefaultBrowser", false);
        user_pref("browser.search.update",             false);
        user_pref("update_notifications.enabled",      false);

        # Allow window.dump calls
        user_pref("browser.dom.window.dump.enabled",   true);

        # Reuse the same window/tab
        user_pref("browser.link.open_newwindow",       1);
      '';

      pullOutPage = wrap {
        name   = "pullOutPage.py";
        paths  = [ python ];
        script = ''
          #!/usr/bin/env python
          import os
          import sys

          text = sys.stdin.read()
          html = 'PRE' in text and 'POST' in text

          if not html:
            sys.stderr.write('\nNo PRE/POST sentinels found\n')

          if (os.getenv('DEBUG') != None) or not html:
            sys.stderr.write('\n' + text + '\n')

          if html:
            print(text.split('PRE')[1].split('POST')[0])
        '';
      };
    };
    script = ''
      #!/usr/bin/env bash
      set -e

         DIR=$(mktemp -d -t 'ff-XXXXX')
      FF_DIR="$DIR/firefox-profile"
        HOME="$DIR/home"
      mkdir "$FF_DIR" "$HOME"
      export HOME

      cp "$ffSettings" "$FF_DIR/user.js"

      function firefoxAlive {
        ps -p "$FF_PID" > /dev/null
      }

      function waitForFirefox {
        for X in $(seq 1 "$1")
        do
          sleep 1
          firefoxAlive || fail "Firefox died"
        done
      }

      function firefoxOpen {
        xwininfo -root -children | grep -i firefox > /dev/null
      }

      function cleanUp {
        if firefoxAlive
        then
          kill "$FF_PID" || true
          sleep 1
        fi

        rm -rf "$DIR" || true

        pids=$(jobs -pr)
        if [[ -n "$pids" ]]; then kill $pids; fi

        exit
      }

      trap cleanUp EXIT

      [[ -n "$TIMEOUT" ]] || TIMEOUT=300

      echo "Opening Firefox on '$URL', using profile dir '$FF_DIR'" 1>&2
      dbus-launch firefox -profile "$FF_DIR" \
                          -no-remote         \
                          -new-instance      \
                          "$URL"             > "$DIR/dump.txt" 2>&1 &
      FF_PID="$!"

      for X in $(seq 1 3)
      do
        sleep 5
        if firefoxOpen; then break; fi
      done

      firefoxOpen || fail "Gave up waiting for firefox to start"

      waitForFirefox 15

      [[ -z "$FF_EXTRA_CODE" ]] || "$FF_EXTRA_CODE"

      echo "Opening Web console" 1>&2
      xdotool key ctrl+shift+K
      waitForFirefox 10

      echo "Extracting body HTML" 1>&2

      # shellcheck disable=SC2016
      xdotool type 'window.dump("PRE");'
      xdotool type 'window.dump(document.documentElement.outerHTML);'
      xdotool type 'window.dump("POST");'
      waitForFirefox 3

      xdotool key --clearmodifiers Return
      sleep 2

      kill "$FF_PID"
      sleep 2
      "$pullOutPage" < "$DIR/dump.txt"
    '';
  };
};

wrap {
  name   = "ff";
  paths  = [ bash coreutils ];
  vars   = {
    inherit ff;
    xvfb     = xvfb-run-safe;
    XVFB_VNC = "1";
  };
  script = ''
    #!/usr/bin/env bash
    # shellcheck disable=SC2154
    URL="$1" timeout 60 "$xvfb" "$ff"
  '';
}
