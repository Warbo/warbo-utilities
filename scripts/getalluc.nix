{ bash, curl, ff, lib, runCommand, vidsfrompage, wget, wrap, writeScript,
  withDeps, xidel }:

with builtins;
with lib;
with rec {
  SITE = "http://www.alluc.ee";

  getalluc = wrap {
    name   = "getalluc";
    paths  = [ bash wget xidel ];
    vars   = { inherit ff SITE vidsfrompage; };
    script = ''
      #!/usr/bin/env bash
      set -e

      [[ -z "$DEBUG" ]] || set -x

      function search {
        # Poor man's URL escaping
        echo "$@" | sed -e 's/ /+/g'
      }

      # Search for commandline arguments and get videos
      Q=$(search "$@")
      URL="$SITE/stream/$Q"

      function allResults {
        xidel - -q --extract '//div[@class="title"]/a/@href'
      }

      function removeAds {
        grep -v "^/source/" | grep -v "luc\.ee#\$"
      }

      function prefixLinks {
        while read -r REL
        do
          echo "$SITE""$REL"
        done
      }

      set -o pipefail
      LINKS=$("$ff" "$URL" | allResults | removeAds | prefixLinks | uniq) ||
        LINKS=""
      set +o pipefail

      echo -e "Search results:\n$LINKS" 1>&2

      while read -r LINK
      do
        echo "Getting vids from page '$LINK'" 1>&2

        # Avoid '.html' as it's often '.avi.html' and other such nonsense.
        # Avoid 'thevideo.me' since their URLs contain Rick Rolls!
        URLS=$("$vidsfrompage" "$LINK" | grep -v '\.html'         |
                                         grep -v '\.thevideo\.me' |
                                         grep -v 'uc\.ee')

        while read -r THIS_URL
        do
          [[ -n "$THIS_URL" ]] || continue

          echo "Got URL '$THIS_URL'" 1>&2
          FIXED=$(echo "$THIS_URL" | sed -e "s/'$//g")

          echo "Checking file type" 1>&2
          RESPONSE=$(wget --server-response --spider "$FIXED" 2>&1) || continue
          TYPE=$(echo "$RESPONSE" | grep 'Content-Type')            || continue
          echo "$TYPE" 1>&2

          if echo "$TYPE" | grep -i 'html' > /dev/null
          then
            continue
          fi

          echo "inDir ~/Public/TODO wget -O '$*' '$FIXED'"

          [[ -z "$STOPONFIRST" ]] || exit 0
        done < <(echo "$URLS")
      done < <(echo "$LINKS")
    '';
  };

  tests = attrValues {
    bigBuckBunny = runCommand "test-big-buck-bunny"
      {
        inherit getalluc SITE;
        STOPONFIRST = "1";  # Short-circuit if we find anything
        buildInputs = [ curl ];
      }
      ''
        set -e

        if curl "$SITE" > /dev/null
        then
          echo "We seem to be online..." 1>&2
        else
          echo "Not online, skipping test" 1>&2
          mkdir "$out"
          exit 0
        fi

        FOUND=$("$getalluc" big buck bunny host:vidzi.tv | tee >(cat >&2))
        if echo "$FOUND" | grep "wget"
        then
          echo "Found video URL" 1>&2
          mkdir "$out"
          exit 0
        fi

        echo "No URL found" 1>&2
        exit 1
      '';
  };
};

withDeps tests getalluc
