{ bash, coreutils, curl, fail, glibc, html2text, makeWrapper, python3,
  runCommand, wget, withDeps, wrap, xidel, xmlstarlet }:

with builtins;
with rec {
  tableToTsv = wrap {
    paths  = [ (python3.withPackages (p: [ p.beautifulsoup4 p.unicodecsv ])) ];
    vars   = { LANG = "en_US.UTF-8"; };
    script = ''
      #!/usr/bin/env python
      from bs4 import BeautifulSoup
      from functools import reduce
      import sys

      def correctTable(table):
        rows = table.find_all('tr')
        if len(rows) < 2:
          return False
        return any(map(correctRow, rows))

      def correctRow(row):
        cells = row.find_all('td')
        if len(cells) != 4:
          return False
        if len(cells[2].text.strip()) != len("2000-01-01"):
          return False
        return True

      def processTable(table):
        rows = list(filter(correctRow, table.find_all('tr')))
        return list(map(processRow, rows))

      def processRow(row):
        cells = row.find_all('td')
        return '\t'.join(list(map(lambda x: x.text, cells[0:3])))

      in_str = sys.stdin.read()
      soup   = BeautifulSoup(in_str, 'html.parser')
      tables = list(filter(correctTable, soup.find_all('table')))
      assert len(tables) > 0, "Got no appropriate tables"

      rowss  = list(map(processTable, tables))
      rows   = reduce(lambda x, y: x + y, rowss, [])
      assert len(rows) > 0, "Got no rows"

      result = '\n'.join(rows)

      print(result)
    '';
  };

  go = wrap {
    name   = "get-eps";
    paths  = [ bash coreutils curl glibc.bin fail html2text wget xidel xmlstarlet ];
    vars   = {
      inherit tableToTsv;
      SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;
    };
    script = ''
      #!/usr/bin/env bash
      set -e

      echo "$2" | grep 'thetvdb.com' > /dev/null ||
        fail "get_eps URL should be from thetvdb.com"

      FEED=$(mktemp '/tmp/get-eps-XXXXX.xml')

      function cleanup {
        rm -f "$FEED"
      }
      trap cleanup EXIT

      PAGE=$(curl "$2") || fail "Couldn't download '$2'"

      # shellcheck disable=SC2154
      EPS=$(echo "$PAGE" | "$tableToTsv") || fail "Couldn't get eps"

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

         NUM=$(echo "$EP" | cut -f1)
        NAME=$(echo "$EP" | cut -f2)
        DATE=$(echo "$EP" | cut -f3)

        SECS=$(date -d "$DATE" '+%s')

        # Anything older than a year is not news
        if [[ -z "$KEEP_ALL" ]] && [[ "$SECS" -lt "$LY"  ]]
        then
          continue
        fi

        # shellcheck disable=SC2001
        URL=$(echo "$2" | sed -e 's/&/&amp;/g')

        # Anything scheduled for the future is no use
        if [[ "$SECS" -lt "$NOW" ]]
        then
          echo "Writing episode $NUM" 1>&2
          xmlstarlet ed -L \
            -a "//channel" -t elem -n item        -v ""             \
            -s "//item[1]" -t elem -n title       -v "$NUM $NAME"   \
            -s "//item[1]" -t elem -n link        -v "$URL"         \
            -s "//item[1]" -t elem -n pubDate     -v "$DATE"        \
            -s "//item[1]" -t elem -n description -v "Episode $NUM" \
            -s "//item[1]" -t elem -n guid        -v "$1-$NUM" "$FEED"
        fi
      done < <(echo "$EPS" | iconv -c -f utf-8 -t ascii//translit |
               sed -e 's/&/&amp;/g')

      cat "$FEED"
    '';
  };

  tests = {
    haveExpanse = runCommand "test-expanse"
    {
      inherit go;
      buildInputs = [ curl fail xidel ];
      KEEP_ALL    = "1";
      URL         = "https://www.thetvdb.com/?tab=seasonall&id=280619&lid=7";
    }
    ''
      curl http://thetvdb.com > /dev/null || {
        echo "WARNING: Couldn't access TVDB (offline?). Skipping test" 1>&2
        mkdir "$out"
        exit
      }
      CONTENT=$("$go" "TheExpanse" "$URL") || fail "Failed to get eps"

      echo "$CONTENT" | xidel -q - -e '//item//pubDate' |
                        grep '2015-12-14' > /dev/null ||
        fail "Expanse s01e01 not found?\n$CONTENT"

      mkdir "$out"
    '';
  };
};
withDeps [ (attrValues tests) ] go
