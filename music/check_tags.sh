#!/usr/bin/env bash

function artistOf {
    A_DIR=$(echo "$1" | grep -o "Music/Commercial/[^/]*/[^/]*")
    basename "$A_DIR"
}

function albumOf {
    A_DIR=$(echo "$1" | grep -o "Music/Commercial/[^/]*/[^/]*/[^/]*")
    basename "$A_DIR"
}

function checkTag {
    GOT=$(echo "$DATA" | grep "^$1" | cut -d ' ' -f 2-)
    [[ "x$GOT" = "x$2" ]] || return 1
}

function checkMp3s {
    command -v id3v2 > /dev/null || {
        echo "Not checking MP3 tags since id3v2 not found" 1>&2
        return
    }
    while read -r F
    do
        ARTIST=$(artistOf "$F")
         ALBUM=$( albumOf "$F")

        DATA=$(id3v2 -R "$F")
        HAS=$(checkTag "TPE2" "$ARTIST") || {
            echo "$F has artist '$HAS', should be '$ARTIST'"
        }
    done < <(find Music/Commercial -type f -iname "*.mp3" | head)
}

checkMp3s
