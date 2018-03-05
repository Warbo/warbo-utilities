{ bash, coreutils, curl, fail, lib, runCommand, sleepDumpPage, vidsfrompage,
  wget, wrap, writeScript, withDeps, xidel, xvfb-run-safe }:

with builtins;
with lib;
with rec {
  SITE = "http://www.alluc.ee";

  getalluc = wrap {
    name   = "getalluc";
    paths  = [ bash coreutils fail wget xidel ];
    vars   = { inherit SITE sleepDumpPage vidsfrompage; };
    script = ''
      #!/usr/bin/env bash
      set -e

      [[ -z "$DEBUG" ]] || set -x

      # Search for commandline arguments. Poor man's URL escaping ' ' -> '+'.
      ARGS="$*"
         Q="${"$" + "{ARGS// /+}"}"
       URL="$SITE/stream/$Q"

      # shellcheck disable=SC2154
      SEARCH_PAGE=$(timeout 300 "$sleepDumpPage" "$URL") ||
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

      DEDUPED=$(echo "$PREFIXED" | sort -u)
      FINAL=$(echo "$DEDUPED" | grep 'zi.tv'; echo "$DEDUPED" | grep -v 'zi.tv')

      echo -e "Search results:\n$FINAL" 1>&2

      while read -r LINK
      do
        echo "Getting vids from page '$LINK'" 1>&2

        # shellcheck disable=SC2154
        URLS=$(timeout 300 "$vidsfrompage" "$LINK") || continue

        echo "$URLS" | grep 'youtube-dl' | grep -v '.thevideo.me' |
          while read -r YT
          do
            echo "$YT" | grep '^.' > /dev/null || continue
            echo "inDir ~/Public/TODO $YT"
            [[ -z "$STOPONFIRST" ]] || exit 0
          done

        URLS=$(echo "$URLS" | grep -v 'youtube-dl')

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
          echo "inDir ~/Public/TODO wget -O '$*' '$FIXED'"

          [[ -z "$STOPONFIRST" ]] || exit 0
        done < <(echo "$FILTERED")
      done < <(echo "$FINAL")
    '';
  };

  tests = attrValues {
    bigBuckBunny = runCommand "test-big-buck-bunny"
      {
        inherit getalluc SITE sleepDumpPage;
        buildInputs = [ curl fail ];
        MESSAGE     = ''
          Testing that getalluc works. This test will be skipped if we can't
          reach the site with curl (e.g. if us or them are offline)
         '';
        STOPONFIRST = "1";  # Short-circuit if we find anything
      }
      ''
        set -e
        set -o pipefail

        echo "$MESSAGE" 1>&2

        if curl -s "$SITE" > /dev/null
        then
          echo "We seem to be online..." 1>&2
        else
          echo "WARNING: Curl failed; assume we're offline, skipped test." 1>&2
          mkdir "$out"
          exit 0
        fi

        if "$sleepDumpPage" "http://example.com" | grep 'body' > /dev/null
        then
          echo "Chromium can get page source..." 1>&2
        else
          fail "Chromium couldn't get example.com page source." 1>&2
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
};

withDeps tests getalluc
