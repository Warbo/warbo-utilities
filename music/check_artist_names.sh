#!/usr/bin/env bash
set -e

# Check each directory at the artist level against external metadata databases

function stripSpace {
    sed -e 's/[[:space:]]*$//g' | sed -e 's/^[[:space:]]*//g'
}

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
        true
    else
        echo "Searching for '$1' on metal-archives.com" 1>&2
        sleep 1
        curl --get --data-urlencode "field=name"      \
             --data-urlencode "query=$1" \
             "http://www.metal-archives.com/search/ajax-band-search/" > "$CACHED"
    fi
    [[ -f "$CACHED" ]] || {
        echo "Error: Couldn't find metal-archives data for '$1'" 1>&2
        return 1
    }
    echo "$CACHED"
}

function haveMetalArchive {
    if echo "$1" | grep '([^)]*)$' > /dev/null
    then
        BANDNAME=$(echo "$1" | rev | cut -d '(' -f 2- | rev | stripSpace)
        COUNTRY=$(echo "$1" | rev | cut -d '(' -f 1  | rev | tr -d ')' | stripSpace)
    else
        BANDNAME="$1"
        COUNTRY=""
    fi

    ARCHIVE=$(metalArchiveFor "$BANDNAME") || return 1
    MATCHES=$(jq '.iTotalRecords' < "$ARCHIVE")
    if [[ "$MATCHES" -eq 1 ]]
    then
        # One match; assume it's exact (for now)
        return 0
    fi

    if [[ "$MATCHES" -eq 0 ]]
    then
         # No matches, so nothing we can do. Remove error message when we have
         # more backends
         echo "Error: No matches for '$BANDNAME' on metal-archives" 1>&2
         return 1
    fi

    if [[ "$MATCHES" -gt 0 ]] && [[ -z "$COUNTRY" ]]
    then
        # We have multiple matches, but no way to distinguish them
        echo "Error: $MATCHES matches for '$BANDNAME' on metal archives (maybe add country?)" 1>&2
        return 1
    fi

    echo "Directory country '$COUNTRY'"
    echo "Result countries: "
    jq '.aaData | map(.[2])' < "$ARCHIVE"
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

    for D2 in "$D1/"*
    do
        assertDir "$D2" || continue

        checkArtistDir "$D2" || {
            echo "Error: Failed to find artist for '$D2'" 1>&2
        }
    done
done
