#!/usr/bin/env bash
set -euo pipefail

## For getting Meetup data

function getPage() {
    FILE="$HOME/.meetup"
    [[ -e "$FILE" ]] || {
        echo "No '$FILE' file found, for meetup.com query URL" 1>&2
        exit 1
    }
    QUERY=$(cat "$FILE")
    [[ -n "$QUERY" ]] || {
        echo "Couldn't find meetup.com search URL in '$FILE'" 1>&2
        exit 2
    }
    wget -q -O- "$QUERY"
}

function getEvents() {
    GETRESULT='ul[contains(@class,"searchResults")]'
    GETEVENT='div[@class="chunk"]//a[contains(@class, "event")]'
    xidel -q - -e "//$GETRESULT//$GETEVENT/@href"
}

function eventPage() {
    SHA256=$(echo "$1" | sha256sum | cut -d ' ' -f1)
    DIR="$HOME/.cache/meetup"
    [[ -e "$DIR" ]] || mkdir -p "$DIR"
    [[ -e "$DIR/$SHA256" ]] || {
        wget -q -O "$DIR/$SHA256" "$1"
    }
    [[ -e "$DIR/$SHA256" ]] || {
        echo "Couldn't get page '$1', aborting" 1>&2
        exit 1
    }
    cat "$DIR/$SHA256"
}

function getGroup() {
    xidel - -q -e '//span[contains(@class, "event-info-group--groupName")]'
}

function getName() {
    xidel - -q -e '//h1'
}

function getDate() {
    MILLIS=$(xidel - -q -e '//time/@dateTime' | head -n1)
    [[ -n "$MILLIS" ]] || {
        echo "Couldn't get date in milliseconds since epoch" 1>&2
        exit 1
    }
    SECS=$(( MILLIS / 1000 ))
    date -R -d "@$SECS"
}

function getDetails() {
    xidel - -q -e '//div[contains(@class, "event-description")]'
}

## Render RSS

function xmlEscape() {
    # From https://stackoverflow.com/a/12873723/884682
    sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g'
}

function rssHeader() {
    echo '<?xml version="1.0" encoding="utf-8"?>'
    echo '<rss version="2.0">'
      echo '<channel>'
        echo '<title>MeetUp</title>'
        echo '<link>https://meetup.com/</link>'
        echo '<description>Scraped from MeetUp.com.</description>'
        echo '<language>en</language>'
}

function rssFooter() {
      echo '</channel>'
    echo '</rss>'
}

function rssItem() {
    # Arguments are:
    #  $1 Event URL
    #  $2 Event name
    #  $3 Event group
    #  $4 Event time
    #  $5 Event details
      XURL=$(echo "$1" | xmlEscape)
    XTITLE=$(echo "$2" | xmlEscape)
    XGROUP=$(echo "$3" | xmlEscape)
     XTIME=$(echo "$4" | xmlEscape)
     XDESC=$(echo "$5" | xmlEscape)

    echo '<item>'
      echo "<title>$XTITLE</title>"
      echo "<link>$XURL</link>"
      echo "<description>$XDESC</description>"
      echo "<pubDate>$XTIME</pubDate>"
      echo "<guid>$XURL</guid>"
      echo "<author>$XGROUP</author>"
    echo '</item>'
}

## Go!

EVENTS=$(getPage | getEvents)

rssHeader

while read -r EVENT
do
     PAGE=$(eventPage "$EVENT")
    GROUP=$(echo "$PAGE" | getGroup  )
     NAME=$(echo "$PAGE" | getName   )
     DATE=$(echo "$PAGE" | getDate   )
     DESC=$(echo "$PAGE" | getDetails)

    rssItem "$EVENT" "$NAME" "$GROUP" "$DATE" "$DESC"
done < <(echo "$EVENTS")

rssFooter
