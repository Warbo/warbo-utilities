{ bash, coreutils, curl, fail, feed2maildirsimple, libxslt, mkBin, mu, openssl,
  python, wget, wrap, xidel, xmlstarlet }:

with rec {
  cleanUp = wrap {
    name = "clean-up-news";
    script = ''
      #!/usr/bin/env bash

      # Some feeds are high-volume and only interesting for a short time. We
      # clean up their articles 1 month after posting
      for FEED in BBCHeadlines HackerNews XKCD SMBC ScottishTech kurzweilai
      do
        CUTOFF=$(date -d "last month" "+%s")
        while read -r F
        do
          D=$(grep "^Date: " < "$F" | sed -e 's/^Date: //g')
          SECS=$(date -d "$D" "+%s")
          if [[ "$SECS" -lt "$CUTOFF" ]]
          then
            rm "$F"
          fi
        done < <(find "$HOME/Mail/feeds/$FEED/cur" -type f)
      done

      # Some feeds may post content that originated a while ago, e.g. the BBC
      # showing films from many years ago. If these are only available for a
      # short time (like iPlayer posts) then we should delete those whose file
      # modification time (rather than posted date) is older than a month
      for FEED in iPlayerComedy iPlayerFilms iPlayerSciNat
      do
        find "$HOME/Mail/feeds/$FEED/cur" -type f -mtime +30 -delete
      done
    '';
  };

  fixRss = mkBin {
    name   = "fixRss";
    paths  = [ xmlstarlet ];
    script = ''
      #!/usr/bin/env bash

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
    '';
  };

  courier = mkBin {
    name   = "courier";
    paths  = [ bash getRss ];
    script = ''
      #!/usr/bin/env bash

      # Scrape the Dundee Courier
      # Edit URL http://feed43.com/feed.html?name=dundee_courier
      COURIER="DundeeCourier.rss"
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
  };

  stripNonAscii = "tr -cd '[:print:]\n'";

  get = "timeout 20 wget -O- -q --no-check-certificate";

  getRss = mkBin {
    name   = "getRss";
    paths  = [ coreutils fixRss wget ];
    script = ''
      #!/usr/bin/env bash
      ${get} "$2" | ${stripNonAscii} | fixRss "$1" > "$1.rss"
    '';
  };

  getAtom = mkBin {
    name   = "getAtom";
    paths  = [ coreutils fixRss libxslt.bin wget ];
    vars   = { xsl = /home/chris/System/Programs/atom2rss-exslt.xsl; };
    script = ''
      #!/usr/bin/env bash
      ${get} "$2" | ${stripNonAscii} > "$1.atom"
      xsltproc "$xsl" "$1.atom" |
        fixRss "$1" > "$1.rss"
    '';
  };

  getYouTube = mkBin {
    name   = "getYouTube";
    paths  = [ getAtom ];
    script = ''
      #!/usr/bin/env bash
      getAtom "$1" "http://www.youtube.com/feeds/videos.xml?channel_id=$2"
    '';
  };

  rss = wrap {
    name   = "pull_down_rss";
    paths  = [ bash getAtom getRss getYouTube ];
    script = ''
      #!/usr/bin/env bash
      set -e
      [[ -n "$1" ]] || fail "pull_down_rss need an output directory"
      [[ -e "$1" ]] || fail "Output dir '$1' not found"
      cd "$1"

      while read -r FEED
      do
        TYPE=$(echo "$FEED" | cut -f1)
        NAME=$(echo "$FEED" | cut -f2)
         URL=$(echo "$FEED" | cut -f3)

        echo "Getting feed $NAME" 1>&2
        case "$TYPE" in
          atom)
            getAtom "$NAME" "$URL" || echo "Failed to get $NAME, skipping"
            ;;
          iplayer)
            iplayer_feed "$NAME" "$URL" > "$NAME.rss" ||
              echo "Failed to get $NAME, skipping"
            ;;
          rss)
            getRss "$NAME" "$URL" || echo "Failed to get $NAME, skipping"
            ;;
          tv)
            get_eps "$NAME" "$URL" > "$NAME.rss" ||
              echo "Failed to get $NAME, skipping"
            ;;
          youtube)
            getYouTube "$NAME" "$URL" || echo "Failed to get $NAME, skipping"
            ;;
          *)
            echo "Can't handle '$FEED', skipping" 1>&2
            ;;
        esac
      done
    '';
  };
};

wrap {
  name   = "get-news-start";
  paths  = [ bash courier fail mu python feed2maildirsimple ];
  vars   = { inherit cleanUp rss; };
  script = ''
    #!/usr/bin/env bash
    set -e

    # Grabs RSS feeds and dumps them in ~/.cache, so all of our news is in one
    # format and one place, ready to merge into our mail
    if /var/setuid-wrappers/ping -c 1 google.com
    then
      # Update all of our RSS files
      mkdir -p ~/.cache/rss
      cd ~/.cache/rss || fail "Couldn't cd to ~/.cache/rss"

      bbcnews > BBCHeadlines.rss ||
        echo "Error getting BBC news, skipping" 1>&2

      courier ||
        echo "Error scraping Dundee Courier, skipping" 1>&2

      "$rss" ~/.cache/rss < ~/.feeds
    fi

    echo "Converting RSS to maildir" 1>&2
    for F in ~/.cache/rss/*.rss
    do
      NAME=$(basename "$F" .rss)
      echo "$NAME" 1>&2
      feed2maildir -s -m ~/Mail/feeds/"$NAME" -n "$NAME" < "$F" ||
        echo "Failed to convert $NAME, skipping" 1>&2
    done

    echo "Cleaning up old news" 1>&2
    "$cleanUp"
  '';
}
