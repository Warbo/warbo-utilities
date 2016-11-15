{ bash, gnugrep, gnused, haskellPackages, libxslt, runCommand, wget,
  makeWrapper, mu, python, writeScript, xidel, xmlstarlet }:

with rec {
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

        listToFeed "iPlayer Comedy" "http://www.bbc.co.uk/iplayer/categories/comedy/all?sort=dateavailable" > "$CACHEDIR/comedy.rss"

        listToFeed "iPlayer Films" "http://www.bbc.co.uk/iplayer/categories/films/all?sort=dateavailable" > "$CACHEDIR/films.rss"

        listToFeed "iPlayer Sci/Nat" "http://www.bbc.co.uk/iplayer/categories/science-and-nature/all?sort=dateavailable" > "$CACHEDIR/scinat.rss"
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

        # Grabs RSS feeds and dumps them in ~/.cache
        # Used to work around things imm doesn't support (e.g. HTTPS)

        function stripNonAscii {
          tr -cd '[:print:]\n'
        }

        function fixRss {
          # Set the author to $1, to avoid newlines
          xmlstarlet ed -u "//author" -v "$1" |

          # Append today as the pubDate, then remove all but the first
          # pubDate (i.e. append today as the pubDate, if none is given)
          xmlstarlet ed          \
            -s //item -t elem -n pubDate             \
            -v "$(date -d "today 00:00" --rfc-2822)" \
            -d '//item/pubDate[position() != 1]'
        }

        function atomToRss {
          xsltproc ~/System/Programs/atom2rss-exslt.xsl "$1.atom" |
            fixRss "$1" > "$1.rss"
        }

        function get {
          timeout 20 wget --no-check-certificate "$@"
        }

        function getAtom {
          get -O - "$2" | stripNonAscii > "$1.atom"
          atomToRss "$1"
        }

        function getYouTube {
          get -O - "http://www.youtube.com/feeds/videos.xml?channel_id=$2" |
          stripNonAscii > "$1.atom"
          atomToRss "$1"
        }

        function getRss {
          get -O - "$2" | stripNonAscii | fixRss "$1" > "$1.rss"
        }

        mkdir -p ~/.cache/rss
        cd ~/.cache/rss || {
          echo "Couldn't cd to ~/.cache/rss" 1>&2
          exit 1
        }

        # Configurable feeds
        while read -r FEED
        do
          TYPE=$(echo "$FEED" | cut -f1)
          NAME=$(echo "$FEED" | cut -f2)
           URL=$(echo "$FEED" | cut -f3)

          case "$TYPE" in
            rss)
              getRss "$NAME" "$URL"
              ;;
            atom)
              getAtom "$NAME" "$URL"
              ;;
            youtube)
              getYouTube "$NAME" "$URL"
              ;;
            *)
              echo "Can't handle '$FEED'" 1>&2
              ;;
          esac
        done < ~/.feeds

        # Scrape BBC iPlayer
        "${iplayer}"

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

  nodupes = runCommand "nodupes"
      {
        buildInputs = [ makeWrapper ];
        raw         = writeScript "remove_dupes" ''
          #!${bash}/bin/bash

          function getField {
            grep "^$1: " | cut -d ' ' -f 2-
          }

          for MSG in ~/Mail/feeds/*/new/*
          do
            # FIXME: Use "from:", but it doesn't handle spaces

            DIR=$(echo "$MSG" | sed -e 's@.*/Mail/feeds/\([^/]*\)/new/.*@\1@g')

            # Our main identifier is the subject
            SUB=$(getField "Subject" < "$MSG")

            # Special characters are tricky. Removing question marks seems to
            # help
            SUB=$(echo "$SUB" | tr -d '?')

            # I can't find a way to escape apostrophes within a word, so we use
            # a wildcard instead, e.g. "Russia's" becomes "Russia*"
            SUB=$(echo "$SUB" | sed -e "s/\(\w\w*\)'\w\w*/\1*/g")

            # Apostrophies outside words, e.g. "over 'bribe' allegations", cause
            # problems, but can be stripped out
            SUB=$(echo "$SUB" | tr -d "'")

            # We wrap the results in quotes, to get term matching
            SUB='"'"$SUB"'"'

            # Delete if this isn't the only unread version
            if FOUND=$(mu find --fields l "maildir:/feeds/$DIR" \
                                          "subject:$SUB"        \
                                          flag:unread 2> /dev/null)
            then
              COUNT=$(echo "$FOUND" | wc -l)
              if [[ "$COUNT" -gt 1 ]]
              then
                rm "$MSG"
                continue
              fi
            fi

            # Delete if this has already been read
            if mu find --fields l "maildir:/feeds/$DIR" \
                                  "subject:$SUB"        \
                                  flag:seen 1>/dev/null 2> /dev/null
            then
              rm "$MSG"
            fi
          done
        '';
      }
      ''
        #!${bash}/bin/bash
        makeWrapper "$raw" "$out" --prefix PATH : "${mu}/bin"     \
                                  --prefix PATH : "${gnused}/bin" \
                                  --prefix PATH : "${gnugrep}/bin"
      '';
};

runCommand "mk-getnews"
  {
    buildInputs = [ makeWrapper ];

    raw = writeScript "get-news-start" ''
      #!${bash}/bin/bash

      function index {
        mu index --maildir=/home/chris/Mail
      }

      if /var/setuid-wrappers/ping -c 1 google.com
      then
        # Run any RSS-generating scripts we might have
        "${rss}"

        pushd ~/.cache
        python -m SimpleHTTPServer 8888 &
        SERVER_PID="$!"
        popd

        # Run imm to send RSS to mailbox
        imm -u

        kill "$SERVER_PID"

        index

        # imm is bad at spotting duplicates, so we remove any
        "${nodupes}"

        index
      fi
    '';
  }
  ''
    #!${bash}/bin/bash
    makeWrapper "$raw" "$out" \
      --prefix PATH : "${mu}/bin"     \
      --prefix PATH : "${python}/bin" \
      --prefix PATH : "${haskellPackages.ghcWithPackages (h: [h.imm])}/bin"
  ''
