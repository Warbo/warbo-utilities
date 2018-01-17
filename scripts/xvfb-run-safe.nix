# xvfb-run has a few annoyances. Most important are that, prior to the end of
# 2017, it redirects stderr to stdout; it also clobbers itself if multiple
# instances are run. We fix this, as well as providing niceties like VNC access.
{ bash, fail, replace, runCommand, utillinux, wrap, x11vnc, xvfb_run }:

with rec {
  # Hack to avoid unwanted quasiquotes
  braced = s: "$" + "{" + s + "}";

  # Patch xvfb_run to stop it merging stderr into stdout
  patched = runCommand "patch-xvfb-run"
    {
      buildInputs = [ fail replace ];
      old         = xvfb_run;
      broken      = ''DISPLAY=:$SERVERNUM XAUTHORITY=$AUTHFILE "$@" 2>&1'';
      fixed       = ''
        if [[ "x$XVFB_VNC" = "x1" ]]
        then
          echo "Starting VNC server, as requested" 1>&2
          DISPLAY=":$SERVERNUM" XAUTHORITY="$AUTHFILE" x11vnc -localhost \
                                                              -quiet 1>&2 &
        fi
        DISPLAY=":$SERVERNUM" XAUTHORITY="$AUTHFILE" "$@"
      '';
    }
    ''
      set -e

      cp -rv "$old" "$out"
      chmod +w -R "$out"

      # Update references, e.g. in makeWrapper scripts
      find "$out" -type f | while read -r FILE
      do
        replace "$old" "$out" -- "$FILE"
      done

      # Look for the script. If it's been through makeWrapper, use the original.
         NAME="xvfb-run"
      WRAPPED="$out/bin/.${braced "NAME"}-wrapped"
       SCRIPT="$out/bin/$NAME"

      if [[ -f "$WRAPPED" ]]
      then
        SCRIPT="$WRAPPED"
      fi

      [[ -f "$SCRIPT" ]] || fail "xvfb-run script '$SCRIPT' not found"

      if grep -F "$broken" < "$SCRIPT"
      then
        echo "Patching broken xvfb-run script" 1>&2
        replace "$broken" "$fixed" -- "$SCRIPT"
      else
        echo "Not patching '$SCRIPT' since it doesn't appear broken" 1>&2
      fi
    '';

  # Wrap xvfb_run, so we can find a free DISPLAY number, etc.
  go = wrap {
    name   = "xvfb-run-safe";
    paths  = [ bash fail utillinux patched x11vnc ];
    script = ''
      #!/usr/bin/env bash
      set -e

      # allow settings to be updated via environment
      : "${braced "xvfb_lockdir:=/tmp/xvfb-locks"}"
      : "${braced "xvfb_display_min:=99"}"
      : "${braced "xvfb_display_max:=599"}"

      PERMISSIONS=$(stat -L -c "%a" "$xvfb_lockdir")
            OCTAL="0$PERMISSIONS"
         WRITABLE=$(( OCTAL & 0002 ))

      if [[ "$WRITABLE" -ne 2 ]]
      then
        echo "ERROR: xvfb_lockdir '$xvfb_lockdir' isn't world writable" 1>&2
        fail "This may cause users to clobber each others' DISPLAY"     1>&2
      fi

      mkdir -p -- "$xvfb_lockdir" ||
        fail "Couldn't make xvfb_lockdir '$xvfb_lockdir'"

      function cleanUp {
        rm -f "$xvfb_lockdir/$i" ||
          echo "Failed to delete xvfb lockfile '$xvfb_lockdir/$i'. Oh well" 1>&2
      }

      # Look for a free DISPLAY number, starting from min and going to max
      i="$xvfb_display_min"
      while (( i < xvfb_display_max ))
      do
        # Skip this number if there's an X display using it
        if [[ -f "/tmp/.X$i-lock" ]]
        then
          (( ++i ))
          continue
        fi

        # Otherwise, try locking a file for this ourselves
        exec 5> "$xvfb_lockdir/$i" || {
          # Skip if e.g. permission denied
          (( ++i ))
          continue
        }

        # Wait for the lock
        if flock -x -n 5
        then
          # We got a lock, make sure we clean up after ourselves
          trap cleanUp EXIT

          # Now run the command we were asked to
          xvfb-run --server-num="$i" "$@"
        fi

        # If we couldn't get the lock (e.g. due to a timeout), try the next
        (( i++ ))
      done
    '';
  };
};

go
