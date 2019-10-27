{ bash, wget, wrap, xidel }:

wrap {
  name   = "iplayer";
  paths  = [ bash wget xidel ];
  script = ''
    #!${bash}/bin/bash
    set -e
    set -o pipefail

    function fetchProgrammes {
      # Fetch the URLs of programmes found at the given URL, followed by their
      # titles. For example:
      #
      # http://programme-page-for-click
      # http://programme-page-for-springwatch
      #
      # Click - 25th May 2016
      # Springwatch 2016 Episode 3

      XPATH="//a[contains(@class,\"content-item__link\")]"

      echo "Fetching '$1'" 1>&2
      wget -q -O- "$1" |
        xidel -q \
          -e "$XPATH/resolve-uri(@href, \"$1\")" \
          -e "$XPATH//div[contains(@class,\"content-item__title\")]/text()" -
    }

    function formattedProgrammes {
      # Fetch the URLs and titles of programmes on the given page. For example:
      #
      # http://programme-page-for-click	Click - 25th May 2016
      # http://programme-page-for-springwatch 	Springwatch 2016 Episode 3

      OUTPUT=$(fetchProgrammes "$1") || fail "Couldn't fetch feed"
      URLS=$(echo "$OUTPUT" | grep "^http")
      TTLS=$(echo "$OUTPUT" | grep -v "^http" | grep "^.")

      assertSameLength "$URLS" "$TTLS"

      paste <(echo "$URLS") <(echo "$TTLS")
    }

    function assertSameLength {
      # Assert that both arguments contain the same number of lines
      COUNT1=$(echo "$1" | wc -l)
      COUNT2=$(echo "$2" | wc -l)

      echo "Got lists of '$COUNT1' and '$COUNT2' elements" 1>&2
      [[ "$COUNT1" -eq "$COUNT2" ]] || {
        echo -e "Found different length lists. First:\\n$1\\nSecond:\\n$2" 1>&2
        exit 2
      }
      [[ "$COUNT1" -gt 3 ]] ||
        fail "Only found '$COUNT' entries? Seems fishy, aborting."
    }

    function listToFeed {
      CHANNELURL=$(echo "$2" | xmlEscape)
      echo '<rss version="2.0">'
      echo   '<channel>'
      echo     "<title>$1</title>"
      echo     "<link>$CHANNELURL</link>"

      FORMATTED=$(formattedProgrammes "$2") || fail "Couldn't format listing"

      COUNT=0
      while read -r LINE
      do
        COUNT=$(( COUNT + 1 ))
        THISURL=$(echo "$LINE" | cut -f 1)
        THISTTL=$(echo "$LINE" | cut -f 2-)
        writeItem "$THISURL" "$THISTTL"
      done < <(echo "$FORMATTED")

      echo   '</channel>'
      echo '</rss>'
    }

    function xmlEscape {
      # From http://daemonforums.org/showthread.php?t=4054
      sed -e 's~&~\&amp;~g' -e 's~<~\&lt;~g' -e 's~>~\&gt;~g'
    }

    function firstShown {
      sleep 1
      PAGE=$(wget -O- -q "$1") || fail "Couldn't fetch page $1"
      EXTRACTED=$(echo "$PAGE" | grep -o '"release_date_time":"[^"]*"') ||
        fail "Failed to extract first-shown date"

      echo "$EXTRACTED" | cut -d : -f 2- |
                          sed -e 's/"//g'
    }

    function findCached {
      MATCHES=$(grep -rlF "$1" "$CACHEDIR")
      FOUND=$(echo "$MATCHES" | grep -v '\.rss$' | head -n1)
      echo "$FOUND"
    }

    function writeItem {
      CACHED=$(findCached "$1")
      if [[ -n "$CACHED" ]]
      then
        cat "$CACHED"
      else
        HASH=$(echo "$1" | md5sum | cut -d ' ' -f 1)
        NAME=$(echo "$2" | tr '[:upper:]' '[:lower:]' | tr -dc '[:lower:]')
        FILE="$HASH"_"$NAME".xml
        writeItemReal "$1" "$2" | tee "$CACHEDIR/$FILE"
      fi
    }

    function writeItemReal {
      echo "Writing item for '$1' '$2'" 1>&2
      SAFEURL=$(echo "$1" | xmlEscape)
      SAFETTL=$(echo "$2" | xmlEscape)
      # Strip off "First shown:" and "HH:MMpm"
      DATE=$(firstShown "$1")
      echo "Got date '$DATE'" 1>&2
      if PUBDATE=$(date --date="$DATE" --rfc-2822)
      then
        # Looks like a complete date
        true
      else
        # Probably just a year, e.g. for a film
        PUBDATE=$(date --date="1 Jan$DATE" --rfc-2822)
      fi
      echo "<item>"
      echo   "<title>$SAFETTL</title>"
      echo   "<link>$SAFEURL</link>"
      echo   "<description><a href=\"$SAFEURL\">link</a></description>"
      echo   "<guid isPermaLink=\"true\">$SAFEURL</guid>"
      echo   "<pubDate>$PUBDATE</pubDate>"
      echo "</item>"
    }

    CACHEDIR="$HOME/.cache/iplayer_feeds"
    mkdir -p "$CACHEDIR"

    listToFeed "$1" "$2" | tr -cd '[:print:]\n'
  '';
}
