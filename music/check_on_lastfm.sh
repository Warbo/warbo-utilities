#!/usr/bin/env bash

function haveLastFm {
    CACHED="$CACHE_DIR/$INIT/$1"
    if [[ -f "$CACHED" ]]
    then
        true
    else
        echo "Looking up '$1' on last.fm" 1>&2
        sleep 1
        ENCODED=$(echo "$1" | tr ' ' '+')
        curl --get "http://www.last.fm/music/$ENCODED" > "$CACHED"
    fi
    if grep "404 - Page Not Found" < "$CACHED" > /dev/null
    then
        echo "Couldn't find '$1' on last.fm" 1>&2
        return 1
    fi
    return 0
}

[[ -n "$INIT" ]] || {
    echo "No INIT set, aborting" 1>&2
    exit 1
}

CACHE_DIR="$PWD/.lastfm_artist_cache"
mkdir -p "$CACHE_DIR/$INIT"

haveLastFm "$1"
