#!/usr/bin/env bash
set -e

BASE=$(dirname "$(readlink -f "$0")")

# Check each directory at the artist level against external metadata databases

function esc {
    sed -e "s/'/'\\\\''/g"
}

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
    if command -v nix-shell > /dev/null
    then
        ARG_ESC=$(echo "$1" | esc)
        nix-shell -p jq --run "'$HMA' '$ARG_ESC'"
    else
        "$HMA" "$1"
    fi
}

function haveLastFm {
    HLF="$BASE/check_on_lastfm.sh"
    [[ -e "$HLF" ]] || {
        echo "Couldn't find script '$HLF', aborting" 1>&2
        exit 1
    }
    "$HLF" "$1"
}

function checkArtistDir {
    ARTIST=$(basename "$1")
    if haveMetalArchive "$ARTIST"
    then
        return 0
    fi
    haveLastFm "$ARTIST"
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
