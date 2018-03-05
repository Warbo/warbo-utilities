{ bash, curl, fail, iframeUrls, jsbeautifier, lib, procps, runCommand,
  sleepDumpPage, wget, withDeps, wrap, xidel }:

with builtins;
with lib;
with rec {
  olcextra = with rec {
    go = wrap {
      name   = "olcextra";
      paths  = [ bash ];
      script = ''
        #!/usr/bin/env bash
        echo '(function(elem) { if (elem == null) { return; } else { elem.click(); } })(document.getElementById("videooverlay"))'
        sleep 10
      '';
    };

    olctest = runCommand "olc-test"
      {
        inherit go sleepDumpPage;
        buildInputs   = [ curl fail xidel ];
        url           = "https://open" + "loa" + "d.co/f/miEoI5oT8JE/";
        SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;
      }
      ''
        curl "$url" > /dev/null || {
          echo "WARNING: Couldn't download page (offline?), skipping test" 1>&2
          mkdir "$out"
          exit 0
        }

        OUTPUT=$(EXTRA="$go" "$sleepDumpPage" "$url") || {
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

  scrapepage = wrap {
    name   = "scrapepage";
    paths  = [ bash curl jsbeautifier xidel ];
    vars   = { inherit olcextra sleepDumpPage; };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Try a basic curl request, to get the raw source
      CONTENT1=$(curl -s "$1") || CONTENT1=""

      if echo "$1" | grep 'ad.co' > /dev/null
      then
        EXTRA="$olcextra"
        export EXTRA
      fi

      # shellcheck disable=SC2154
      CONTENT2=$("$sleepDumpPage" "$1") || CONTENT2=""

      # Try to de-obfuscate Javascript with js-beautify, to find more URLs
      CONTENT3=""
      EVALTEMP=$(echo -e "$CONTENT1"                           |
                 xidel -q - -e '//script[contains(., "eval")]' |
                 js-beautify -i)
      CONTENT3=$(echo -e "$CONTENT3\n$EVALTEMP")

      EVALTEMP=$(echo -e "$CONTENT2"                           |
                 xidel -q - -e '//script[contains(., "eval")]' |
                 js-beautify -i)
      CONTENT3=$(echo -e "$CONTENT3\n$EVALTEMP")

      # Take everything we've got and look for video URLs
      CONTENT=$(echo -e "$CONTENT1\n$CONTENT2\n$CONTENT3")

      VIDSRC1=$(echo "$CONTENT1" | xidel -q - -e '//video/@src') || true
      VIDSRC2=$(echo "$CONTENT2" | xidel -q - -e '//video/@src') || true

      BASE=$(echo "$1" | sed -e 's#[^/]*//\([^@]*@\)\?\([^:/]*\).*#\2#')

      VS1ABS=$(echo "$VIDSRC1" | grep    '^http'                       ) || true
      VS1REL=$(echo "$VIDSRC1" | grep -v '^http' | sed -e "s#^#$BASE#g") || true
      VS2ABS=$(echo "$VIDSRC2" | grep    '^http'                       ) || true
      VS2REL=$(echo "$VIDSRC2" | grep -v '^http' | sed -e "s#^#$BASE#g") || true

      VIDSRC1=$(echo -e "$VSABS1\n$VSREL1" | grep '^.') || true
      VIDSRC2=$(echo -e "$VSABS2\n$VSREL2" | grep '^.') || true

      VIDURLS=""
      for VID in mp4 flv avi
      do
        VIDS=$(echo "$CONTENT" | grep -i "\.$VID") || true
        NARROWED=""
        while read -r CANDIDATE
        do
          # If we find '"foo.$VID"' then use that, otherwise use the whole line
          NARROW=$(echo "$CANDIDATE" | grep -vo "\"[^\"]*\.$VID[^\"]*\"") ||
            true
          [[ -n "$NARROW" ]] || NARROW="$CANDIDATE"

          NARROWED=$(echo -e "$NARROWED\n$NARROW")
        done < <(echo "$VIDS")

        VIDURLS=$(echo -e "$VIDURLS\n$NARROWED")
      done

      set +e
      # Try to narrow-down output lines to just URLs
      OUTURLS=$(echo -e "$VIDSRC1\n$VIDSRC2\n$VIDURLS" |
                grep -o 'http[^ "<>]*'                 |
                grep -o '[^"]*'                        |
                grep -v 'ads.ad-center.com')
      set -e

      echo "$OUTURLS" | sort -u
    '';
  };
};

wrap {
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
      IFRAMES=$("$iframeUrls" "$1") || break

      LINKS2=$(echo "$IFRAMES" | grep '^http') || break

      echo -e "Found iframes for:\n$LINKS2" 1>&2
      break
    done

    # Loop over result links, getting videos and obfuscated javascript
    while read -r LNK
    do
      [[ -n "$LNK" ]] || continue

      SKIP=0
      for PAT in recaptcha "luc.ee#" addthis
      do
        if echo "$LNK" | grep "$PAT" > /dev/null
        then
          SKIP=1
        fi
      done
      if [[ "$SKIP" -eq 1 ]]; then continue; fi

      # Worth a try...
      echo "youtube-dl \"$LNK\""

      echo "Scraping page '$LNK'" 1>&2

      # Generic scraper
      # shellcheck disable=SC2154
      "$scrapepage" "$LNK" || continue
    done < <(echo "$LINKS2")
  '';
}
