#!/usr/bin/env bash
set -e

# Check each directory at the artist level against external metadata databases

function assertDir {
    [[ -d "$1" ]] || {
        echo "Error: '$1' isn't a directory" 1>&2
        return 1
    }
    return 0
}

function metalArchiveFor {
    CACHED="$CACHE_DIR/$INIT/$1"
    if [[ -f "$CACHED" ]]
    then
        echo "Using cache for '$1'" 1>&2
    else
        echo "Searching for '$1' on metal-archives.com" 1>&2
        sleep 1
        curl --get --data-urlencode "field=name"      \
             --data-urlencode "query=$1" \
             "http://www.metal-archives.com/search/ajax-band-search/" > "$CACHED"
    fi
    [[ -f "$CACHED" ]] || {
        echo "Couldn't find metal-archives data for '$1'" 1>&2
        return 1
    }
    echo "$CACHED"
}

function haveMetalArchive {
    ARCHIVE=$(metalArchiveFor "$1") || return 1
    MATCHES=$(jq '.iTotalRecords' < "$ARCHIVE")
    if [[ "$MATCHES" -eq 1 ]]
    then
        echo "Found '$ARTIST' on metal-archives.com" 1>&2
        return 0
    fi

    echo "Found '$MATCHES' results for '$ARTIST'" 1>&2
    return 1
}

function checkArtistDir {
    ARTIST=$(basename "$1")
    haveMetalArchive "$ARTIST"
}

CACHE_DIR="$PWD/.artist_name_cache"
mkdir -p "$CACHE_DIR"

for D1 in Music/Commercial/*
do
    assertDir "$D1" || continue
    INIT=$(basename "$D1")
    mkdir -p "$CACHE_DIR/$INIT"

    echo "Checking $INIT..." 1>&2

    for D2 in "$D1/"*
    do
        assertDir "$D2" || continue

        checkArtistDir "$D2" || {
            echo "Error: Failed to find artist for '$D2'" 1>&2
        }
    done
done
