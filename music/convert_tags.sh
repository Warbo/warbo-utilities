#!/usr/bin/env bash

command -v mp3info > /dev/null || {
    echo "No mp3info, aborting" 1>&2
    exit 1
}

command -v mid3v2 > /dev/null || {
    echo "No mid3v2, aborting" 1>&2
    exit 1
}

function hasV1 {
    if mp3info "$1" 2>&1 | grep "does not have an ID3 1.x tag" > /dev/null
    then
        return 1
    fi
    return 0
}

find Music -iname "*.mp3" | while read -r F
do
    hasV1 "$F" || continue

    # Convert id3v1 to id3v2
    mid3v2 --convert "$F"

    # Remove id3v1 tags
    mid3v2 --delete-v1 "$F"

    sync

    if hasV1 "$F"
    then
        echo "Aborting: Couldn't remove ID3v1 tags from $F" 1>&2
        exit 1
    fi
done
