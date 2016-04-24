#!/usr/bin/env bash
set -e

# Check each directory at the artist level against MusicBrainz

CACHE_DIR="$PWD/.artist_name_cache"
mkdir -p "$CACHE_DIR"

for D1 in Music/Commercial/*
do
    echo "$D1"
    continue
    [[ -d "$D1" ]] || {
        echo "Error: '$D1' isn't a directory" 1>&2
        continue
    }
    INIT=$(basename "$D1")
    mkdir -p "$CACHE_DIR/$INIT"

    for D2 in "$D1"
    do
        true
    done
done
