#!/usr/bin/env bash
set -e

# Check each directory at the artist level against MusicBrainz

CACHE_DIR="$PWD/.artist_name_cache"
mkdir -p "$CACHE_DIR"

for D1 in Music/Commercial/*
do
    [[ -d "$D1" ]] || {
        echo "Error: '$D1' isn't a directory" 1>&2
        continue
    }
    INIT=$(basename "$D1")
    mkdir -p "$CACHE_DIR/$INIT"

    echo "Checking $INIT..." 1>&2

    for D2 in "$D1/"*
    do
        [[ -d "$D2" ]] || {
            echo "Error: '$D2' isn't a directory" 1>&2
            continue
        }

        ARTIST=$(basename "$D2")
        CACHED="$CACHE_DIR/$INIT/$ARTIST"
        if [[ -f "$CACHED" ]]
        then
            echo "Using cache for '$ARTIST'" 1>&2
        else
            echo "Looking up '$ARTIST'" 1>&2
            sleep 2
            curl --get --data-urlencode "query=artist:$ARTIST" \
                 "http://musicbrainz.org/ws/2/artist/" > "$CACHED"
            exit 0
        fi
    done
done
