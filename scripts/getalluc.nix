{ bash, coreutils, curl, fail, ff, lib, runCommand, vidsfrompage, wget, wrap,
  writeScript, withDeps, xidel, xvfb-run-safe }:

with builtins;
with lib;
with rec {
  SITE = "http://www.alluc.ee";

  inner = wrap {
    name   = "getalluc";
    paths  = [ bash coreutils fail wget xidel ];
    vars   = { inherit ff SITE vidsfrompage; };
    script = ''
      #!/usr/bin/env bash
      set -e

      [[ -z "$DEBUG" ]] || set -x

      # Search for commandline arguments. Poor man's URL escaping ' ' -> '+'.
      ARGS="$*"
         Q="${"$" + "{ARGS// /+}"}"
       URL="$SITE/stream/$Q"

      # shellcheck disable=SC2154
      SEARCH_PAGE=$(timeout 300 "$ff" "$URL") ||
        fail "Failed to load search page"

      LINKS=$(echo "$SEARCH_PAGE" |
              xidel - -q --extract '//div[@class="title"]/a/@href') ||
        fail "Couldn't extract search results"

      # Disable errexit temporarily, since we don't care if grep "fails"
      set +e
      STRIPPED=$(echo "$LINKS" | grep -v "^/source/" | grep -v "^#")
      set -e

      # shellcheck disable=SC2001
      PREFIXED=$(echo "$STRIPPED" | sed -e "s@^@$SITE@g")

      FINAL=$(echo "$PREFIXED" | sort -u)

      echo -e "Search results:\n$FINAL" 1>&2

      while read -r LINK
      do
        echo "Getting vids from page '$LINK'" 1>&2

        # shellcheck disable=SC2154
        URLS=$(timeout 300 "$vidsfrompage" "$LINK") || continue

        # Avoid '.html' as it's often '.avi.html' and other such nonsense.
        # Avoid 'thevideo.me' since their URLs contain Rick Rolls!
        set +e
        FILTERED=$(echo "$URLS" | grep -v '\.html'         |
                                  grep -v '\.thevideo\.me' |
                                  grep -v 'uc\.ee')
        set -e

        while read -r THIS_URL
        do
          [[ -n "$THIS_URL" ]] || continue

          echo "Got URL '$THIS_URL'" 1>&2
          FIXED="${"$" + "{THIS_URL%\\'}"}"  # Drop any ' from end

          echo "Checking file type" 1>&2
          RESPONSE=$(timeout 60 wget --server-response \
                                     --spider "$FIXED" 2>&1) || continue
          TYPE=$(echo "$RESPONSE" | grep 'Content-Type')     || continue
          echo "$TYPE" 1>&2

          SKIP=0
          for UNWANTED in html javascript jpeg png gif icon mpegurl
          do
            if echo "$TYPE" | grep -i "$UNWANTED" > /dev/null
            then
              SKIP=1
            fi
          done

          [[ "$SKIP" -eq 0 ]] || continue

          echo "inDir ~/Public/TODO wget -O '$*' '$FIXED'"

          [[ -z "$STOPONFIRST" ]] || exit 0
        done < <(echo "$FILTERED")
      done < <(echo "$FINAL")
    '';
  };

  getalluc = wrap {
    name   = "with-display";
    vars   = {
      inherit inner;
      xvfb = xvfb-run-safe;
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      if [[ -n "$EXISTING_DISPLAY" ]]
      then
        # shellcheck disable=SC2154
        exec "$inner" "$@"
      else
        export EXISTING_DISPLAY=1
        # shellcheck disable=SC2154
        exec "$xvfb" "$inner" "$@"
      fi
    '';
  };

  tests = attrValues {
    bigBuckBunny = runCommand "test-big-buck-bunny"
      (if getEnv "DEBUG_UI" == ""
          then {}
          else {
            DISPLAY          = getEnv "DISPLAY";
            EXISTING_DISPLAY = "1";
            XAUTHORITY       = getEnv "XAUTHORITY";
          } // {
        inherit SITE;
        buildInputs = [ curl fail ];
        DEBUG       = builtins.getEnv "DEBUG";
        MESSAGE     = ''
          Testing that getalluc works:
           - This test will be skipped if we can't reach the site with curl
             (e.g. if us or them are offline)
           - For more verbose output, set the DEBUG env var to 1 (it will be
             inherited by the builder)
           - To start an x11vnc server in the xvfb screens, set the XVFB_VNC env
             var to 1 (it will be inherited by the builder)
           - To automatically connect to such x11vnc servers, try having the
             pollvnc script running
         '';
        xvfb     = xvfb-run-safe;
        XVFB_VNC = builtins.getEnv "XVFB_VNC";

        inDisplay = wrap {
          name   = "in-display";
          vars   = {
            inherit ff getalluc;
            EXISTING_DISPLAY = "1";
            STOPONFIRST = "1";  # Short-circuit if we find anything
          };
          script = ''
            #!/usr/bin/env bash
            set -e

            if "$ff" "http://example.com" | grep 'body' > /dev/null
            then
              echo "Firefox can get page source..." 1>&2
            else
              fail "Firefox couldn't get example.com page source." 1>&2
            fi

            if "$getalluc" big buck bunny | grep "wget"
            then
              echo "Found video URL" 1>&2
              mkdir "$out"
              exit 0
            fi

            fail "No URL found"
          '';
        };
      })
      ''
        set -e
        set -o pipefail

        echo "$MESSAGE" 1>&2

        if curl "$SITE" > /dev/null
        then
          echo "We seem to be online..." 1>&2
        else
          echo "WARNING: Curl failed; assume we're offline, skipped test." 1>&2
          mkdir "$out"
          exit 0
        fi

        if [[ -n "$EXISTING_DISPLAY" ]]
        then
          "$inDisplay"
        else
          export EXISTING_DISPLAY=1
          "$xvfb" "$inDisplay"
        fi
      '';
  };
};

withDeps tests getalluc
