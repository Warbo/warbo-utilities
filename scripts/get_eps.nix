{ bash, coreutils, curl, fail, glibc, html2text, runCommand, wget, withDeps,
  wrap, xidel, xmlstarlet }:

with builtins;
with rec {
  go = wrap {
    name   = "get-eps";
    paths  = [ bash coreutils curl glibc.bin wget xmlstarlet ];
    vars   = { SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt; };
    script = ''
      #!/usr/bin/env bash
      set -e

      echo "$2" | grep 'epguides.com' > /dev/null ||
        fail 'get_eps URL should be from epguides.com'

      curl -s -f 'http://epguides.com' > /dev/null ||
        fail "Can't contact epguides.com, aborting"

      FEED=$(mktemp '/tmp/get-eps-XXXXX.xml')

      function cleanup {
        rm -f "$FEED"
      }
      trap cleanup EXIT

      PAGE=$(curl -f "$2") || fail "Couldn't download '$2'"

      cat <<EOF > "$FEED"
      <?xml version="1.0" encoding="utf-8"?>
      <rss xmlns:atom="http://www.w3.org/2005/Atom" version="2.0">
        <channel>
          <title>$1</title>
          <description>$1</description>
        </channel>
      </rss>
      EOF

      NOW=$(date -d 'yesterday' '+%s')
       LY=$(date -d 'last year' '+%s')

      while read -r EP
      do
        echo "$EP" | grep '^.' > /dev/null || continue
        echo "EP: $EP" 1>&2

        # Format is number,season,episode,airdate,title
        DATE=$(echo "$EP" | cut -d , -f 4)

        SECS=$(date -d "$DATE" '+%s')
        PDAT=$(date -d "$DATE" --iso-8601)

        # Anything older than a year is not news
        if [[ -z "$KEEP_ALL" ]] && [[ "$SECS" -lt "$LY"  ]]
        then
          continue
        fi

        # Anything scheduled for the future is no use
        [[ "$SECS" -lt "$NOW" ]] || continue

        # Format is number,season,episode,airdate,title
         NUM=$(echo "$EP" | cut -d , -f 1)
        SNUM=$(echo "$EP" | cut -d , -f 2)
        ENUM=$(echo "$EP" | cut -d , -f 3)
        NAME=$(echo "$EP" | cut -d , -f 5)

        # shellcheck disable=SC2001
        URL=$(echo "$2" | sed -e 's/&/&amp;/g')

         DESC="Episode $NUM, ${"s$" + "{SNUM}e$" + "{ENUM}"} - $NAME"
        TITLE="$NUM (s''${SNUM}e''${ENUM}) $NAME"
        xmlstarlet ed -L \
          -a "//channel" -t elem -n item        -v ""       \
          -s "//item[1]" -t elem -n title       -v "$TITLE" \
          -s "//item[1]" -t elem -n link        -v "$URL"   \
          -s "//item[1]" -t elem -n pubDate     -v "$PDAT"  \
          -s "//item[1]" -t elem -n description -v "$DESC"  \
          -s "//item[1]" -t elem -n guid        -v "$1-$NUM" "$FEED"

      done < <(echo "$PAGE" | grep -v '^\s*<'       |
               iconv -c -f utf-8 -t ascii//translit |
               grep '\S' | grep '^[0-9]')

      cat "$FEED"
    '';
  };

  tests = {
    haveExpanse = runCommand "test-expanse"
      {
        inherit go;
        buildInputs = [ curl fail xidel ];
        KEEP_ALL    = "1";
        URL         = "http://epguides.com/common/exportToCSVmaze.asp?maze=1825";
      }
      ''
        curl -s http://epguides.com > /dev/null || {
          echo "WARNING: Couldn't access epguides (offline?). Skipping test" 1>&2
          mkdir "$out"
          exit
        }
        CONTENT=$("$go" "TheExpanse" "$URL") || fail "Failed to get eps"

        echo "$CONTENT" | xidel -q - -e '//item//pubDate' |
                          grep '2015-12-14' > /dev/null ||
          fail "Expanse s01e01 not found?\n$CONTENT"

        mkdir "$out"
      '';

    haveWalkingDead = runCommand "test-walking-dead"
      {
        inherit go;
        buildInputs = [ curl fail xidel ];
        KEEP_ALL    = "1";
        URL         = "http://epguides.com/common/exportToCSVmaze.asp?maze=73";
      }
      ''
        curl -s http://epguides.com > /dev/null || {
          echo "WARNING: Couldn't access epguides (offline?). Skipping test" 1>&2
          mkdir "$out"
          exit
        }
        CONTENT=$("$go" "WalkingDead" "$URL") || fail "Failed to get eps"

        echo "$CONTENT" | xidel -q - -e '//item//title' |
          grep 'What Comes After' > /dev/null ||
          fail "Walking Dead s09e05 not found?\n$CONTENT"

        mkdir "$out"
      '';
  };
};
withDeps [ (attrValues tests) ] go
