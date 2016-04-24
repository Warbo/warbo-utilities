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
        echo "$D2"
        break
        [[ -d "$D2" ]] || {
            echo "Error: '$D2' isn't a directory" 1>&2
            continue
        }
    done
    break
done
