{ bash, coreutils, curl, fail, iframeUrls, jsbeautifier, lib, procps,
  runCommand, sleepDumpPage, wget, withDeps, wrap, xidel }:

with builtins;
with lib;
with rec {
  testUrl = "https://open" + "loa" + "d.co/f/miEoI5oT8JE/";

  SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;

  olcextra = with rec {
    go = wrap {
      name   = "olcextra";
      paths  = [ bash ];
      script = ''
        #!/usr/bin/env bash
        echo '(function(elem) { if (elem == null) { return; } else { elem.click(); } })(document.getElementById("videooverlay"))'
        for _ in $(seq 1 10)
        do
          printf "." 1>&2
          sleep 1
        done
      '';
    };

    olctest = runCommand "olc-test"
      {
        inherit go sleepDumpPage SSL_CERT_FILE testUrl;
        buildInputs   = [ curl fail xidel ];
      }
      ''
        curl -s "$testUrl" > /dev/null || {
          echo "WARNING: Couldn't download page (offline?), skipping test" 1>&2
          mkdir "$out"
          exit 0
        }

        OUTPUT=$(EXTRA="$go" "$sleepDumpPage" "$testUrl") || {
          echo "OUTPUT: $OUTPUT" 1>&2
          fail "Couldn't get page"
        }

        URLS=$(echo "$OUTPUT" | xidel -q -e '//video/@src' -)
        echo "URLS: $URLS" 1>&2

        echo "$URLS" | grep '^.' > /dev/null || fail "No URLs found"

        mkdir "$out"
      '';
  };
  withDeps [ olctest ] go;

  scrapepage =
    with rec {
      scrapeSource = wrap {
        name   = "scrape-source";
        paths  = [ bash xidel ];
        script = ''
          #!/usr/bin/env bash

          GIVEN=$(cat)

          VIDSRC=$(echo "$GIVEN"  | xidel -q - -e '//video/@src') || true
          echo "$VIDSRC" | grep    '^http'
          echo "$VIDSRC" | grep -v '^http' | while read -r REL
          do
            echo "$REL" | grep '^.' > /dev/null || continue
            echo "$1$REL"
          done

          for VID in mp4 flv avi
          do
            VIDS=$(echo "$GIVEN" | grep -i "\.$VID") || true
            while read -r CANDIDATE
            do
              # If we find '"foo.$VID"' then use that, otherwise the whole line
              NARROW=$(echo "$CANDIDATE" | grep -vo "\"[^\"]*\.$VID[^\"]*\"")
              [[ -n "$NARROW" ]] || NARROW="$CANDIDATE"

              echo "$NARROW"
            done < <(echo "$VIDS")
          done

          exit 0
        '';
      };

      go = wrap {
        name   = "scrapepage";
        paths  = [ bash curl jsbeautifier xidel ];
        vars   = { inherit olcextra scrapeSource sleepDumpPage SSL_CERT_FILE; };
        script = ''
          #!/usr/bin/env bash

          # Get base URL (e.g. http://foo.com/bar gives http://foo.com)
          BASE=$(echo "$1" | sed -e 's#\(https*://[^/]*\).*#\1#')

          function go {
            [[ -z "$CONTENT" ]] || echo "$CONTENT" | "$scrapeSource" "$BASE" |
                                                     grep -o 'http[^ "<>]*'  |
                                                     grep -o '[^"]*'         |
                                                     grep -v 'ad-center.com' |
                                                     sort -u
          }

          # Site-specific approaches
          if echo "$1" | grep 'dzi.tv' > /dev/null
          then
            echo "youtube-dl '$1'"
            exit 0
          fi

          if echo "$1" | grep 'embed2.php' > /dev/null
          then
            CONTENT=$(curl -s "$1")
            while read -r URL
            do
              echo "$URL" | grep '^.' > /dev/null || continue
              echo "youtube-dl $URL"
            done < <(echo "$CONTENT" | xidel -q -e '//a/@href' - | grep 'zi.tv')
            exit 0
          fi

          if echo "$1" | grep 'ad.co' > /dev/null
          then
            # shellcheck disable=SC2154
            CONTENT=$(EXTRA="$olcextra" "$sleepDumpPage" "$1")
            go
            exit 0
          fi

          # Try a basic curl request, to get the raw source
          CONTENT=$(curl -s "$1") || true
          go

          # Try a headless browser, in case JS needs to be run
          # shellcheck disable=SC2154
          CONTENT=$("$sleepDumpPage" "$1") || true
          go
          exit 0
        '';
      };

      scrapeTest = runCommand "scrape-test"
        {
          inherit go SSL_CERT_FILE testUrl;
          buildInputs = [ curl fail ];
        }
        ''
          curl -s "$testUrl" > /dev/null || {
            echo "WARNING: Offline? Skipping test" 1>&2
            mkdir "$out"
            exit 0
          }

          OUTPUT=$("$go" "$testUrl" | grep -v '\.jpg') || true
          echo "$OUTPUT" | grep '^http' > /dev/null || {
            echo "OUTPUT: $OUTPUT" 1>&2
            fail "No URL found"
          }
          echo "OUTPUT: $OUTPUT" 1>&2
          mkdir "$out"
        '';
    };
    withDeps [ scrapeTest ] go;

  vfp = wrap {
    name   = "vidsfrompage";
    paths  = [ bash procps wget xidel ];
    vars   = {
      inherit iframeUrls scrapepage;
    };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Look for raw URLs
      CONTENT1=$(wget -q -O- "$1") || CONTENT1=""
      LINKS1=$(echo "$CONTENT1" | grep -o 'http[^ "]*mp4') || LINKS1=""
      echo "$LINKS1"

      # Runs at most one iteration; allows us to break early
      LINKS2=""
      while true
      do
        # shellcheck disable=SC2154
        IFRAMES=$("$iframeUrls" "$1") || break

        LINKS2=$(echo "$IFRAMES" | grep '^http') || break

        echo -e "Found iframes for:\n$LINKS2" 1>&2
        break
      done

      # Loop over result links, getting videos
      while read -r LNK
      do
        [[ -n "$LNK" ]] || continue

        SKIP=0
        for PAT in recaptcha "luc.ee#" addthis thevideo.me
        do
          if echo "$LNK" | grep "$PAT" > /dev/null
          then
            SKIP=1
          fi
        done
        if [[ "$SKIP" -eq 1 ]]; then continue; fi

        echo "Scraping page '$LNK'" 1>&2
        # shellcheck disable=SC2154
        "$scrapepage" "$LNK" || continue
      done < <(echo "$LINKS2")
    '';
  };

  vfpTest = runCommand "vfp-test"
    (rec {
      inherit SSL_CERT_FILE vfp;
      DOMAIN      = "http://www.al" + "luc.ee";
      URL         = DOMAIN + "/l/Sintel-2010-HD/rkpaclyq";
      buildInputs = [ curl fail ];
    })
    ''
      curl -s "$DOMAIN" > /dev/null || {
        echo "WARNING: Skipping test (offline?)" 1>&2
        mkdir "$out"
        exit 0
      }

      FOUND=$("$vfp" "$URL")
      echo "$FOUND" | grep '^youtube-dl' || {
        echo "FOUND: $FOUND" 1>&2
        fail "No youtube-dl command found"
      }
      mkdir "$out"
    '';
};

withDeps [ vfpTest ] (wrap {
  name  = "vids-from-page-wrapper";
  vars  = { inherit SSL_CERT_FILE vfp; };
  paths = [ bash coreutils wget ];
  script = ''
    #!/usr/bin/env bash
    # shellcheck disable=SC2154
    "$vfp" "$@" | while read -r URL
    do
      if echo "$URL" | grep '^youtube-dl ' > /dev/null
      then
        echo "$URL"
        continue
      fi

      printf "Checking file type: " 1>&2
      RESPONSE=$(timeout 60 wget --server-response \
                                 --spider "$URL" 2>&1) || {
        echo "Failed to get server response, skipping" 1>&2
        continue
      }
      TYPE=$(echo "$RESPONSE" | grep 'Content-Type') || {
        echo "Not found, skipping" 1>&2
        continue
      }
      printf '%s' "$TYPE" 1>&2

      SKIP=0
      for UNWANTED in html javascript jpeg png gif icon mpegurl
      do
        if echo "$TYPE" | grep -i "$UNWANTED" > /dev/null
        then
          SKIP=1
        fi
      done

      [[ "$SKIP" -eq 0 ]] || {
        echo ". Not acceptable, skipping" 1>&2
        continue
      }
      echo ". Acceptable, keeping" 1>&2
      echo "$URL"
    done
  '';
})
