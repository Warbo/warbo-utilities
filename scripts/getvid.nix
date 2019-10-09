{ bash, fail, jq, jsbeautifier, lynx, raw, wget, wrap, writeScript, xidel,
  youtube-dl }:

with rec {
  f5 = wrap {
    name  = "getvid-f5";
    paths = [ bash jsbeautifier xidel ];
    script = ''
      #!/usr/bin/env bash
      set -e
      wget -q -O- "$1"                                                  |
        xidel -q - -e '//script[contains(text(),"p,a,c,k,e,d")]/text()' |
        js-beautify -                                                   |
        grep -v '\.srt"'                                                |
        grep -o 'file: *"[^"]*'                                         |
        grep -o '".*'                                                   |
        tr -d '"'                                                       |
        head -n1
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

  ytdl = wrap {
    name   = "getvid-ytdl";
    paths  = [ bash youtube-dl ];
    script = ''
      #!/usr/bin/env bash
      set -e
      URL="$1"
      if youtube-dl -s "$1" 2>&1 > /dev/null
      then
        echo "$1"
        exit 0
      fi
      exit 1
    '';
  };
};
wrap {
  name  = "getvid";
  paths = [ bash xidel ];
  vars  = {
    inherit f5 voza vse ytdl;
    list = raw."listepurls.sh";
    msg  = ''
      Usage: getvid <listing url>

      Looks through a listing of providers, printing 'TITLE\tURL' to stderr for
      each. Loops through each to see if (a) it has a handler (youtube-dl or
      custom) and (b) whether the handler returns a working URL. If so, a
      command for fetching from that provider is written to stdout.
      Set DEBUG=1 to see each handler running.

      Known handlers (e.g. for running standalone) are:
        ${f5}
        ${voza}
        ${vse}
        ${ytdl}
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

    function tryScrape {
         LINK="$1"
          TIT="$2"
        REGEX="$3"
      SCRAPER="$4"
          CMD="$5"

      echo "$LINK" | grep "$REGEX" > /dev/null || return 1

      [[ -n "$DEBUG" ]] && echo "Running $SCRAPER on $LINK" 1>&2
      URL=$("$SCRAPER" "$LINK") || return 0

      [[ -n "$URL" ]] || return 0
      URL=$(echo "$URL" | esc)

      [[ "x$CMD" = "xwget"    ]] && P="wget -c -O"
      [[ "x$CMD" = "xyoutube" ]] && P="youtube-dl --output"
      echo "$P '$TIT' --no-check-certificate '$URL'"
      return 0
    }

    echo "$LINKS" | while read -r PAIR
    do
      LINK=$(echo "$PAIR" | cut -f2)
      [[ -n "$LINK" ]] || continue

      TITLE=$(echo "$PAIR" | cut -f1 | esc)
      [[ -n "$TITLE" ]] || TITLE="UNKNOWN"

      URL=""

      [[ -n "$DEBUG" ]] && echo "Checking $LINK" 1>&2

      # shellcheck disable=SC2154
      tryScrape "$LINK" "$TITLE" '.' "$ytdl" 'youtube' && continue

      # shellcheck disable=SC2154
      tryScrape "$LINK" "$TITLE" 'x5[4-6][4-6]\.c' "$f5"   'wget'    && continue

      # shellcheck disable=SC2154
      tryScrape "$LINK" "$TITLE" '/vi..z.\.net/'   "$voza" 'wget'    && continue

      # shellcheck disable=SC2154
      tryScrape "$LINK" "$TITLE" '/vs...e\.e'      "$vse"  'wget'    && continue
    done
  '';
}
