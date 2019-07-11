#!/usr/bin/env bash
set -e

# Fetch or generate feeds which we can't get normally (e.g. if they need
# extra postprocessing)

DESTINATION="$1"

[[ -n "$DESTINATION" ]] ||
    fail "get_dodgy_feeds needs the destination directory as an argument"

[[ -e "$DESTINATION" ]] ||
    fail "get_dodgy_feeds argument needs to be a directory"

function xid {
    # Xidel with common options
    xidel -q - "$@"
}

function esc {
    # XML escape
    sed -e 's/&/&amp;/g'  \
        -e 's/"/&quot;/g' \
        -e "s/'/&apos;/g" \
        -e 's/>/&gt;/g'   \
        -e 's/</&lt;/g'
}

function get_bastards {
    # There is an RSS feed for this, from iHeartRadio, but it doesn't have
    # 'link' elements that we rely on for grabbing the mp3. Hence we generate
    # our own, based on the homepage.
    echo "Generating BehindTheBastards feed" 1>&2
    U="https://www.behindthebastards.com/podcasts/behind-the-bastards-archive.htm"
    ARCHIVE=$(wget -q -O- "$U") || {
        echo "Couldn't fetch BehindTheBastards archive, skipping" 1>&2
        return
    }

    # Plucks out each episode's section
    XPATH='//*[contains(@data-track-gtm, "Archive")]'

    # Count how many dates we have
    COUNT=$(echo "$ARCHIVE" | xid -e "$XPATH/div/p" | grep -c '^.')

    [[ "$COUNT" -gt 0 ]] || {
        echo "Got no BehindTheBastards episodes, skipping" 1>&2
        return
    }

    # Preamble
    echo '<?xml version="1.0" encoding="UTF-8"?>
      <rss xmlns:atom="http://www.w3.org/2005/Atom" version="2.0">
        <channel>
          <atom:link href="https://feeds.megaphone.fm/behindthebastards"
                     rel="self"
                     type="application/rss+xml"/>
          <title>Behind the Bastards</title>
          <link>https://www.behindthebastards.com/</link>
          <language>en</language>
          <copyright/>
          <description>Generated from archive page</description>'

    # Loop through the episode indices [1, 2, ..., $COUNT]
    for I in $(seq 1 "$COUNT")
    do
         DATE=$(echo "$ARCHIVE" | xid -e "($XPATH/div/p)[$I]"   )
        TITLE=$(echo "$ARCHIVE" | xid -e "($XPATH//a/p)[$I]"    | esc)
         PAGE=$(echo "$ARCHIVE" | xid -e "($XPATH//a/@href)[$I]")

        # The page contains a player embedded in an iframe
        CONTENT=$(wget -O- -q "$PAGE") || continue
        URL=$(echo "$CONTENT" | xid -e '//iframe/@src' | grep 'iheart' | esc)

        PUBDATE=$(date -d "$DATE" --rfc-822 | esc)

        echo "<item>
          <title>$TITLE</title>
          <pubDate>$PUBDATE</pubDate>
          <link>$URL</link>
          <description>$TITLE</description>
          <guid isPermaLink='true'>$URL</guid>
          <author>BehindTheBastards</author>
        </item>"
    done

    echo '</channel>
    </rss>'
}

get_bastards > "$DESTINATION/BehindTheBastards.rss"
