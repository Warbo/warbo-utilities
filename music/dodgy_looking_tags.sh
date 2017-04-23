#!/usr/bin/env bash

function checkTags {
    while read -r LINE
    do
        if printf "%s'" "$LINE" | grep " '$"
        then
            echo "$F has tag ending in whitespace"
        fi

        for DODGY in "music.download.com"
        do
            if echo "$LINE" | grep -F "$DODGY"
            then
                echo "$1 has dodgy tag"
            fi
        done
    done
}

function doMp3 {
    find Music/Commercial -iname "*.mp3" | while read -r F
    do
        printf '.' 1>&2
        mid3v2 --list "$F" | checkTags "$F"
    done
}

if command -v mid3v2 > /dev/null
then
    echo "Found mid3v2, checking MP3s" 1>&2
    doMp3 2>(fold -w 10 1>&2)
else
    echo "Not checking MP3 tags since mid3v2 not found" 1>&2
fi
