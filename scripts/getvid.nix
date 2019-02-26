{ bash, lynx, raw, wget, wrap, writeScript, xidel }:

with rec {
  sv = wrap {
    name   = "getvid-sv";
    paths  = [ bash wget ];
    script = ''
      #!/usr/bin/env bash
      if wget -q -O - "$1" > /dev/null
      then
        echo "$1"
        exit 0
      fi
      exit 1
    '';
  };

  voza = wrap {
    name   = "getvid-voza";
    paths  = [ bash wget xidel ];
    script = ''
      #!/usr/bin/env bash
      URL=$(wget -q -O - "$1" | xidel -q -e '//video/source/@src' -)
      echo "$URL" | grep 'http' && exit 0
      exit 1
    '';
  };

  vse = wrap {
    name  = "getvid-vse";
    paths = [ bash lynx ];
    vars  = {
      COLUMNS = "1000";
      cmd     = writeScript "vse-keys" ''
        # Command logfile created by Lynx 2.8.9dev.16 (11 Jul 2017)

        # Submit form
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key ^J

        # View source
        key \
        key y

        # Print to screen
        key p
        key Down Arrow
        key Down Arrow
        key ^J

        # Confirm
        key y
        key ^J

        # Exit
        key ^J
        key q
        key y
      '';
    };
    script = ''
      #!/usr/bin/env bash
      URL=$(lynx -term=linux -accept_all_cookies -cmd_script="$cmd" "$1" |
            grep 'video/mp4' | grep -o 'http[^"]*')
      echo "$URL" | grep 'http' && exit 0
      exit 1
    '';
  };
};
wrap {
  name  = "getvid";
  paths = [ bash xidel ];
  vars  = {
    inherit sv voza vse;
    list = raw."listepurls.sh";
    msg  = ''
      Usage: getvid <listing url>

      Looks through a listing of providers, printing 'TITLE\tURL' to stderr for
      each. Loops through each to see if (a) it has a handler and (b) whether
      the handler returns a working URL. If so, a command for fetching from that
      provider is written to stdout. Set DEBUG=1 to see each handler running.

      Known handlers (e.g. for running standalone) are:
        ${sv}
        ${voza}
        ${vse}
    '';
  };
  script = ''
    #!/usr/bin/env bash
    set -e

    if [[ "x$1" = "x--help" ]]
    then
      # shellcheck disable=SC2154
      echo "$msg" 1>&2
      exit 0
    fi

    echo "Run with --help as the only arg to see usage and handler scripts" 1>&2

    function esc {
      # shellcheck disable=SC1003
      sed -e "s/'/'"'\\'"'''/g"
    }

    # shellcheck disable=SC2154
    LINKS=$("$list" "$@")

    echo "LINKS: $LINKS" 1>&2

    echo "$LINKS" | while read -r PAIR
    do
      LINK=$(echo "$PAIR" | cut -f2)
      [[ -n "$LINK" ]] || continue

      TITLE=$(echo "$PAIR" | cut -f1 | esc)
      [[ -n "$TITLE" ]] || TITLE="UNKNOWN"

      URL=""

      [[ -n "$DEBUG" ]] && echo "Checking $LINK" 1>&2

      if echo "$LINK" | grep '/spe....d\.co' > /dev/null
      then
        # shellcheck disable=SC2154
        [[ -n "$DEBUG" ]] && echo "Running $sv on $LINK" 1>&2

        # shellcheck disable=SC2154
        URL=$("$sv" "$LINK") || continue

        [[ -n "$URL" ]] || continue
        URL=$(echo "$URL" | esc)

        echo "youtube-dl --output '$TITLE' '$URL'"
        continue
      fi

      if echo "$LINK" | grep '/vi..z.\.net/' > /dev/null
      then
        # shellcheck disable=SC2154
        [[ -n "$DEBUG" ]] && echo "Running $voza on $LINK" 1>&2

        # shellcheck disable=SC2154
        URL=$("$voza" "$LINK") || continue

        [[ -n "$URL" ]] || continue
        URL=$(echo "$URL" | esc)

        echo "wget -c -O '$TITLE' '$URL'"
        continue
      fi

      if echo "$LINK" | grep '/vs...e\.e' > /dev/null
      then
        # shellcheck disable=SC2154
        [[ -n "$DEBUG" ]] && echo "Running $vse on $LINK" 1>&2

        # shellcheck disable=SC2154
        URL=$("$vse" "$LINK") || true

        [[ -n "$URL" ]] || continue
        URL=$(echo "$URL" | esc)

        echo "wget -c -O '$TITLE' '$URL'"
        continue
      fi
    done
  '';
}
