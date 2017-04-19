{ bash, feed2maildirsimple, gnugrep, gnused, libxslt, runCommand, wget,
  makeWrapper, mu, python, writeScript, xidel, xmlstarlet }:

with rec {
  checkTV = writeScript "checkTV" ''
    #!${bash}/bin/bash

    NAME="$1"
     URL="$2"

    CONTENT=$(curl "$URL")

     SERIES=$(echo "$CONTENT" | grep -io "series\W0-9[0-9]*"  | tr '[:upper:]' '[:lower:]')
     SEASON=$(echo "$CONTENT" | grep -io "season\W0-9[0-9]*"  | tr '[:upper:]' '[:lower:]')
    EPISODE=$(echo "$CONTENT" | grep -io "episode\W0-9[0-9]*" | tr '[:upper:]' '[:lower:]')

    S=""
    while read -r LINE
    do
      S="$LINE"
    done < <(printf '%s\n%s' "$SERIES" "$SEASON" | grep '^.')

    E=""
    while read -r LINE
    do
      E="$LINE"
    done < <(echo "$EPISODE" | grep '^.')

    [[ -n "$S" ]] || {
      echo "No series found for '$1'" 1>&2
      exit 1
    }

    [[ -n "$E" ]] || {
      echo "No episode found for '$1'" 1>&2
      exit 1
    }

    echo "$S $E"
  '';

  iplayer = runCommand "mk-iplayer"
    {
      buildInputs = [ makeWrapper ];
      raw = writeScript "iplayer_to_rss" ''
        #!${bash}/bin/bash
        set -e

        function fetchProgrammes {
          # Fetch the URLs of programmes found at the given URL, followed by their
          # titles. For example:
          #
          # http://programme-page-for-click
          # http://programme-page-for-springwatch
          #
          # Click - 25th May 2016
          # Springwatch 2016 Episode 3
          echo "Fetching '$1'" 1>&2
          xidel -q \
                -e '//li[contains(@class,"programme")]/div/a/resolve-uri(@href)' \
                -e '//li[contains(@class,"programme")]/div/a/@title' \
                "$1"
        }

        function formattedProgrammes {
          # Fetch the URLs and titles of programmes on the given page. For example:
          #
          # http://programme-page-for-click	Click - 25th May 2016
          # http://programme-page-for-springwatch 	Springwatch 2016 Episode 3

          OUTPUT=$(fetchProgrammes "$1")
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
            echo -e "Found different length lists. First:\n$1\n\nSecond:\n$2" 1>&2
            exit 2
          }
        }

        function listToFeed {
          listToFeed2 "$@" | tr -cd '[:print:]\n'
        }

        function listToFeed2 {
          CHANNELURL=$(echo "$2" | xmlEscape)
          echo '<rss version="2.0">'
          echo   '<channel>'
          echo     "<title>$1</title>"
          echo     "<link>$CHANNELURL</link>"

          while read -r LINE
          do
            THISURL=$(echo "$LINE" | cut -f 1)
            THISTTL=$(echo "$LINE" | cut -f 2-)
            writeItem "$THISURL" "$THISTTL"
          done < <(formattedProgrammes "$2")

          echo   '</channel>'
          echo '</rss>'
        }

        function xmlEscape {
          # From http://daemonforums.org/showthread.php?t=4054
          sed -e 's~&~\&amp;~g' -e 's~<~\&lt;~g' -e 's~>~\&gt;~g'
        }

        function firstShown {
          sleep 1
          xidel -e '//span[@class="release"]' "$1"
        }

        function findCached {
          MATCHES=$(grep -rlF "$1" "$CACHEDIR")
          FOUND=$(echo "$MATCHES" | grep -v "\.rss$" | head -n1)
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
          DATE=$(firstShown "$1" | cut -d : -f 2- |
                                   sed -e 's/[0-9][0-9]*:[0-9][0-9].m//g' |
                                   sed -e 's/^[ ]*//g')
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

        listToFeed "$1" "$2"
      '';
    }
    ''
      #!${bash}/bin/bash
      makeWrapper "$raw" "$out" --prefix PATH : "${xidel}/bin"
    '';

  rss = runCommand "mk-rss"
    {
      buildInputs = [ makeWrapper ];
      raw = writeScript "pull_down_rss" ''
        #!${bash}/bin/bash

        # Grabs RSS feeds and dumps them in ~/.cache, so all of our news is in
        # one format and one place, ready to merge into our mail

        function stripNonAscii {
          tr -cd '[:print:]\n'
        }

        function fixRss {
          # Append an author to each item, using the feed name
          xmlstarlet ed                         \
            -s //item -t elem -n author         \
            -v "$1"                             \
            -d '//item/author[position() != 1]' |

          # Now that all items have an author, set them all to the feed name (to
          # avoid special characters)
          xmlstarlet ed -u "//author" -v "$1" |

          # Append today as the items' pubDate, then remove all but the first
          # pubDate (i.e. append today as the pubDate, if none is given)
          xmlstarlet ed                              \
            -s //item -t elem -n pubDate             \
            -v "$(date -d "today 00:00" --rfc-2822)" \
            -d '//item/pubDate[position() != 1]'
        }

        function atomToRss {
          xsltproc ~/System/Programs/atom2rss-exslt.xsl "$1.atom" |
            fixRss "$1" > "$1.rss"
        }

        function get {
          timeout 20 wget -O- -q --no-check-certificate "$@"
        }

        function getAtom {
          get "$2" | stripNonAscii > "$1.atom"
          atomToRss "$1"
        }

        function getYouTube {
          get "http://www.youtube.com/feeds/videos.xml?channel_id=$2" |
          stripNonAscii > "$1.atom"
          atomToRss "$1"
        }

        function getRss {
          get "$2" | stripNonAscii | fixRss "$1" > "$1.rss"
        }

        mkdir -p ~/.cache/rss
        cd ~/.cache/rss || {
          echo "Couldn't cd to ~/.cache/rss" 1>&2
          exit 1
        }

        bbcnews > BBCHeadlines.rss

        # Configurable feeds
        while read -r FEED
        do
          TYPE=$(echo "$FEED" | cut -f1)
          NAME=$(echo "$FEED" | cut -f2)
           URL=$(echo "$FEED" | cut -f3)

          case "$TYPE" in
            atom)
              getAtom "$NAME" "$URL"
              ;;
            iplayer)
              "${iplayer}" "$NAME" "$URL" > "$NAME.rss"
              ;;
            rss)
              getRss "$NAME" "$URL"
              ;;
            tv)
              get_eps "$NAME" "$URL" > "$NAME.rss"
              ;;
            youtube)
              getYouTube "$NAME" "$URL"
              ;;
            *)
              echo "Can't handle '$FEED'" 1>&2
              ;;
          esac
        done < ~/.feeds

        # Scrape the Dundee Courier
        # Edit URL http://feed43.com/feed.html?name=dundee_courier
        COURIER="$HOME/.cache/rss/DundeeCourier.rss"
        if [[ -e "$COURIER" ]]
        then
          # Feed43 don't like polling more than every 6 hours
          if test "$(find "$COURIER" -mmin +360)"
          then
            getRss "DundeeCourier" "http://feed43.com/dundee_courier.xml"
          fi
        else
          getRss "DundeeCourier" "http://feed43.com/dundee_courier.xml"
        fi


      '';
    }
    ''
      #!${bash}/bin/bash
      makeWrapper "$raw" "$out" --prefix PATH : "${wget}/bin"       \
                                --prefix PATH : "${xmlstarlet}/bin" \
                                --prefix PATH : "${libxslt.bin}/bin"
    '';
};

runCommand "mk-getnews"
  {
    buildInputs = [ makeWrapper ];

    raw = writeScript "get-news-start" ''
      #!${bash}/bin/bash

      if /var/setuid-wrappers/ping -c 1 google.com
      then
        # Update all of our RSS files
        "${rss}"
      fi

      echo "Converting RSS to maildir" 1>&2
      for F in ~/.cache/rss/*.rss
      do
        NAME=$(basename "$F" .rss)
        echo "$NAME" 1>&2
        feed2maildir -s -m ~/Mail/feeds/"$NAME" -n "$NAME" < "$F"
      done
    '';
  }
  ''
    #!${bash}/bin/bash
    makeWrapper "$raw" "$out"         \
      --prefix PATH : "${mu}/bin"     \
      --prefix PATH : "${python}/bin" \
      --prefix PATH : "${feed2maildirsimple}/bin"
  ''
