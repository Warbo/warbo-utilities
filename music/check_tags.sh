#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function esc {
    "$BASE/esc.sh"
}

function initOf {
    I_DIR=$(echo "$1" | grep -o "Music/Commercial/[^/]*")
    basename "$I_DIR"
}

function artistOf {
    A_DIR=$(echo "$1" | grep -o "Music/Commercial/[^/]*/[^/]*")
    basename "$A_DIR"
}

function albumOf {
    A_DIR=$(echo "$1" | grep -o "Music/Commercial/[^/]*/[^/]*/[^/]*")
    basename "$A_DIR"
}

function checkTag {
    GOT=$(echo "$DATA" | grep "^$1=" | cut -d '=' -f 2-)
    [[ "x$GOT" = "x$2" ]] || {
        echo "$GOT"
        return 1
    }
}

function tagsFor {
    # Artist
    ART_ESC=$(echo "$1" | esc)

    echo -e "TPE1\t$1\t--artist='$ART_ESC'"
    #echo -e "TPE2\t$1"

    # Album
    ALB_ESC=$(echo "$2" | esc)
    echo -e "TALB\t$2\t--album='$ALB_ESC'"
}

function checkMp3s {
    command -v mid3v2 > /dev/null || {
        echo "Not checking MP3 tags since mid3v2 not found" 1>&2
        return
    }
    while read -r F
    do
          INIT=$(initOf   "$F")
        ARTIST=$(artistOf "$F")
         ALBUM=$(albumOf  "$F")

        [[ -d "Music/Commercial/$INIT"                ]] || {
            echo "Couldn't get initial for '$F', skipping" 1>&2
            continue
        }

        [[ -d "Music/Commercial/$INIT/$ARTIST"        ]] || {
            echo "Couldn't get artist for '$F', skipping" 1>&2
            continue
        }

        [[ -d "Music/Commercial/$INIT/$ARTIST/$ALBUM" ]] || {
            echo "Couldn't get album for '$F'" 1>&2
            ALBUM="NONE"
        }

         F_ESC=$(echo "$F" | esc)

        # Normalise the ID3 data, so it's all available as v2, and read it in
        mid3v2 --convert "$F"
        DATA=$(mid3v2 --list "$F")

        while read -r LINE
        do
            FIELD=$(echo "$LINE" | cut -f 1)
              VAL=$(echo "$LINE" | cut -f 2)
              FIX=$(echo "$LINE" | cut -f 3)

            if [[ "x$VAL" = "xNONE" ]]
            then
                continue
            fi

            HAS=$(checkTag "$FIELD" "$VAL") || {
                echo "$F has '$FIELD' of '$HAS', should be '$VAL'"
                echo "mid3v2 $FIX '$F'"
            }
        done < <(tagsFor "$ARTIST" "$ALBUM")
    done < <(find Music/Commercial -type f -iname "*.mp3")
}

checkMp3s
