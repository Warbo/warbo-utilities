#!/usr/bin/env bash
set -e

BASE=$(dirname "$(readlink -f "$0")")

# Check each directory at the artist level against external metadata databases

function assertDir {
    [[ -d "$1" ]] || {
        echo "Error: '$1' isn't a directory" 1>&2
        return 1
    }
    return 0
}

function haveMetalArchive {
    HMA="$BASE/check_on_metalarchive.sh"
    [[ -e "$HMA" ]] || {
        echo "Couldn't find script '$HMA', aborting" 1>&2
        exit 1
    }
    "$HMA" "$1"
}

function checkArtistDir {
    ARTIST=$(basename "$1")
    haveMetalArchive "$ARTIST"
}

for D1 in Music/Commercial/*
do
    assertDir "$D1" || continue
    INIT=$(basename "$D1")
    export INIT

    for D2 in "$D1/"*
    do
        assertDir "$D2" || continue

        checkArtistDir "$D2" || {
            echo "Error: Failed to find artist for '$D2'" 1>&2
        }
    done
done
