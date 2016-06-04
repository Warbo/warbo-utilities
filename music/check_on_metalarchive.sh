#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function metalArchiveTracks {
    [[ -f "$1" ]] || {
        echo "Can't get tracks for '$2' ('$3') since '$1' doesn't exist" 1>&2
        return 1
    }

    TRACK_CACHE="$CACHE_DIR/$INIT/$2_$3.tracks"
    mkdir -p "$TRACK_CACHE"

    while read -r ALBUM_URL
    do
        [[ -n "$ALBUM_URL" ]] || continue

        TRACK_CACHE_FILE=$(echo "$ALBUM_URL" | tr -c '[[:alnum:]]' '_')
        TRACK_FILE="$TRACK_CACHE/$TRACK_CACHE_FILE"

        if [[ -f "$TRACK_FILE" ]]
        then
            true
        else
            echo "Getting tracks from '$ALBUM_URL'" 1>&2
            sleep 1
            curl "$ALBUM_URL" > "$TRACK_FILE" ||
                echo "Error fetching '$ALBUM_URL'" 1>&2
        fi
    done < <(xidel -q - -e '//td/a[@class="album"]/@href' < "$1" | grep '^http')
}

function metalArchiveAlbums {
    ALBUM_CACHE="$CACHE_DIR/$INIT/$2_$3.albums"
    mkdir -p "$CACHE_DIR/$INIT"

    if [[ -f "$ALBUM_CACHE" ]]
    then
        true
    else
        echo "Getting albums for '$2' ('$3') from metal-archives.com" 1>&2
        sleep 1
        BAND_URL=$(jq -r '.aaData[0][0]' < "$1" |
                   grep -o '\".*\"'             |
                   grep -o '[^"]*')
        BAND_ID=$(echo "$BAND_URL" | grep -o '[0-9]*$')

        DISC_URL="http://www.metal-archives.com/band/discography/id/${BAND_ID}/tab/all"
        curl "$DISC_URL" > "$ALBUM_CACHE" || {
            echo "Error fetching '$DISC_URL'" 1>&2
            return 1
        }
    fi
    metalArchiveTracks "$ALBUM_CACHE" "$2" "$3"
}

function metalArchiveFor {
    CACHED="$CACHE_DIR/$INIT/$1"
    mkdir -p "$CACHE_DIR/$INIT"
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
    NAME_CNT=$("$BASE/dir_to_artist_country.sh" "$1")
    BANDNAME=$(echo "$NAME_CNT" | cut -f1)
    CNT=$(echo "$NAME_CNT" | cut -f2)

    # Look up the band on metal archives
    ARCHIVE=$(metalArchiveFor "$BANDNAME") || return 1

    # If we found one match, assume it's exact (for now)
    MATCHES=$(jq '.iTotalRecords' < "$ARCHIVE")
    if [[ "$MATCHES" -eq 1 ]]
    then
        metalArchiveAlbums "$ARCHIVE" "$BANDNAME" "$CNT"
        return 0
    fi

    # If we found no matches, there's nothing we can do
    if [[ "$MATCHES" -eq 0 ]]
    then
        # Remove error message when we have more backends
         echo "Error: No matches for '$BANDNAME' on metal-archives" 1>&2
         return 1
    fi

    # Nothing to distinguish between multiple matches
    if [[ "$MATCHES" -gt 0 ]] && [[ -z "$CNT" ]]
    then
        echo "Error: $MATCHES matches for '$BANDNAME' on metal archives (maybe add country?)" 1>&2
        return 1
    fi

    # See if we have any matching countries
    CNTRESULTS=$(jq ".aaData | map(select(.[2] == \"$CNT\"))" < "$ARCHIVE")
    CNTMATCHES=$(echo "$CNTRESULTS" | jq 'length')
    if [[ "$CNTMATCHES" -eq 1 ]]
    then
        metalArchiveAlbums "$ARCHIVE" "$BANDNAME" "$CNT"
        return 0
    fi

    if [[ "$CNTMATCHES" -eq 0 ]]
    then
        echo "Error: No results for '$BANDNAME' from '$CNT'" 1>&2
        return 1
    fi

    echo "Error: $CNTMATCHES matches for '$BANDNAME' from '$CNT'" 1>&2
    return 1
}

[[ -n "$INIT" ]] || {
    echo "No INIT set, aborting" 1>&2
    exit 1
}

CACHE_DIR="$PWD/.artist_name_cache"
mkdir -p "$CACHE_DIR"

haveMetalArchive "$1"
