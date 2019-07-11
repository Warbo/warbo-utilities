{ bash, coreutils, feed2maildirsimple, libxslt, mkBin, mu-standalone, openssl,
  procps, python, raw, scripts, wget, wrap, writeScript, xidel, xmlstarlet }:

with rec {
  cleanUp = wrap {
    name   = "clean-up-news";
    paths  = [ bash mu-standalone xidel ];
    script = ''
      #!/usr/bin/env bash

      function stopMu {
        while ps auxww | grep 'mu server' | grep -v grep | grep 'server'
        do
          pkill -2 -u "$UID" -f 'mu server'
          sleep 1
        done
      }

      function removeMessage {
        stopMu
        rm "$1"
        mu remove "$1"
      }

      # Some feeds are high-volume and only interesting for a short time. We
      # clean up their articles 1 month after posting
      for FEED in BBCHeadlines HackerNews XKCD SMBC
      do
        CUTOFF=$(date -d "last month" "+%s")
        while read -r F
        do
          D=$(grep "^Date: " < "$F" | sed -e 's/^Date: //g')
          SECS=$(date -d "$D" "+%s")
          if [[ "$SECS" -lt "$CUTOFF" ]]
          then
            removeMessage "$F"
          fi
        done < <(find "$HOME/Mail/feeds/$FEED/cur" -type f)
      done

      # Some feeds may post content that originated a while ago, e.g. the BBC
      # showing films from many years ago. If these are only available for a
      # short time (like iPlayer posts) then we should delete those whose file
      # modification time (rather than posted date) is older than a month
      for FEED in iPlayerComedy iPlayerFilms iPlayerSciNat
      do
        while read -r F
        do
          removeMessage "$F"
        done < <(find "$HOME/Mail/feeds/$FEED/cur" -type f -mtime +30)
      done

      # Limit Reddit feeds to 100 messages. They only include about the latest
      # 25 posts, so we shouldn't get any dupes creeping in.
      for FEED in RedditHaskell RedditStallmanWasRight \
                  ScienceBulletin BBCHeadlines HackerNews
      do
        while read -r F
        do
          removeMessage "$F"
        done < <(mu find --fields="l" --sortfield='d' --reverse \
                         maildir:/feeds/"$FEED" not flag:unread |
                 tail -n+100                                    |
                 head -n20                                      )
      done

      # Delete old BBC news content (since their RSS only has summaries)
      find /tmp/bbcnews-cached -type f -mtime +30 -exec rm {} \;

      # Delete MeetUp events from the past. These may be posted well in advance,
      # so we can't use the file's modification time.
      for F in "$HOME/.cache/meetup"/*
      do
        MILLIS=$(xidel - -q -e '//time/@dateTime' < "$F" | head -n1)
          SECS=$(( MILLIS / 1000 ))
          PAST=$(date -d 'now - 7 days' '+%s')
        (( PAST < SECS )) || rm -f "$F"
        unset MILLIS
        unset SECS
        unset PAST
      done

      stopMu
      mu index --maildir="$HOME/Mail" --lazy-check
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
    paths  = [ coreutils fixRss (libxslt.bin or libxslt) wget ];
    vars   = { xsl = raw."atom2rss-exslt.xsl"; };
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
  paths  = [ bash mu-standalone procps python feed2maildirsimple ];
  vars   = { inherit cleanUp rss; inherit (scripts) sysPing; };
  script = ''
    #!/usr/bin/env bash
    set -e

    # Grabs RSS feeds and dumps them in ~/.cache, so all of our news is in one
    # format and one place, ready to merge into our mail
    # shellcheck disable=SC2154
    if "$sysPing" -c 1 google.com
    then
      # Update all of our RSS files
      mkdir -p ~/.cache/rss
      cd ~/.cache/rss || fail "Couldn't cd to ~/.cache/rss"

      bbcnews > BBCHeadlines.rss ||
        echo "Error getting BBC news, skipping" 1>&2

      # Our MeetUp scraper performs a query each time, so limit how often it
      # runs (it does cache event details)
      ACCEPTABLE_MEETUP=$(date -d 'now - 4 hours' '+%s')
      if [[ -e meetup.rss ]]
      then
        LAST_MEETUP=$(date -r meetup.rss '+%s')
      else
        LAST_MEETUP=0
      fi

      if (( LAST_MEETUP <= ACCEPTABLE_MEETUP ))
      then
        getmeetup > meetup.rss ||
          echo "Error getting meetup events" 1>&2
      fi

      # shellcheck disable=SC2154
      "$rss" ~/.cache/rss < ~/.feeds

      get_dodgy_feeds ~/.cache/rss
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
    # shellcheck disable=SC2154
    "$cleanUp"

    # Re-index (after stopping any existing instance, e.g. the server for mu4e)
    pkill -2 -u "$UID" mu
    sleep 1
    mu index --maildir="$HOME/Mail" --lazy-check
  '';
}
