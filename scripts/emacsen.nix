{ bash, fail, wrap, xdotool }:

wrap {
  name   = "emacsen";
  paths  = [ bash fail xdotool ];
  vars   = {
    DISPLAY = ":0";
  };
  script = ''
    #!/usr/bin/env bash
    set -e

    COUNTDOWN=30

    MSG=0
    while ! pgrep -f 'emacs --daemon' > /dev/null
    do
      [[ "$MSG" -eq 1 ]] ||
        echo "Waiting for Emacs daemon (is system service running?)" 1>&2
      MSG=1
      sleep 1

      [[ "$COUNTDOWN" -gt 1 ]] || fail "Timed out waiting for daemon"
      COUNTDOWN=$(( COUNTDOWN - 1 ))
    done
    unset MSG

    COUNTDOWN=10
    while true
    do
      N=$(pgrep emacsclient | wc -l) || N=0
      [[ "$N" -lt 3 ]] || break
      echo "Found $N emacsclients running, starting one more" 1>&2
      emacsclient -c &
      sleep 2

      [[ "$COUNTDOWN" -gt 1 ]] || fail "Failed too many times"
      COUNTDOWN=$(( COUNTDOWN - 1 ))
    done

    COUNTDOWN=10
    while true
    do
      COUNT=$(xdotool search --classname Emacs || echo "")
      COUNT=$(echo "$COUNT" | wc -l)
      [[ "$COUNT" -gt 2 ]] && break

      sleep 3

      [[ "$COUNTDOWN" -gt 1 ]] || break
      COUNTDOWN=$(( COUNTDOWN - 1 ))
    done
    unset COUNT

    function emacsOn {
      # Whether or not there's an emacs window on the given desktop
      xdotool search --desktop "$1" --classname Emacs || echo ""
    }

    for DESKTOP in 2 3
    do
      sleep 1
      WID=$(emacsOn "$DESKTOP")
      echo "Found '$WID' on '$DESKTOP'" 1>&2
      if [[ -z "$WID" ]]
      then
        # Assume we can move one from super+2
        WID=$(emacsOn 1 | head -n1)
        echo "Fall back to '$WID' on '$DESKTOP'" 1>&2
        [[ -n "$WID" ]] || fail "No emacsclient windows left to move"
      fi
      echo xdotool set_desktop_for_window "$WID" "$DESKTOP" 1>&2
      xdotool set_desktop_for_window "$WID" "$DESKTOP"
    done
  '';
}
