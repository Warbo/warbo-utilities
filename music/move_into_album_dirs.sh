#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function esc {
    "$BASE/esc.sh"
}


TOP=$(readlink -f "Music/Commercial")
for I in Music/Commercial/*
do
    [[ -d "$I" ]] || continue
    INIT=$(basename "$I")
    for A in "$I"/*
    do
        [[ -d "$A" ]] || continue
        ARTIST=$(basename "$A")
        while read -r F
        do
            ALBUM=""
            if echo "$F" | grep -i '\.mp3' > /dev/null
            then
                if command -v mid3v2 > /dev/null 2> /dev/null
                then
                    ALBUM=$(mid3v2 -l "$F" | grep '^TALB=' | cut -d '=' -f2-)
                fi
            fi
            [[ -n "$ALBUM" ]] || continue

            DIR=$(dirname "$(readlink -f "$F")")
            [[ "x$DIR" = "x$TOP/$INIT/$ARTIST/ALBUM" ]] || {
                EF=$(echo "$F"                        | esc)
                EA=$(echo "$ARTIST"                   | esc)
                EB=$(echo "$ALBUM"                    | esc)
                EP=$(echo "$TOP/$INIT/$ARTIST/$ALBUM" | esc)

                echo "File '$F' has album '$ALBUM'; move with command:"
                echo "mkdir -p '$EP'"
                echo "mv '$EF' '$EP/'"
            }
        done < <(find "$A" -type f)
    done
done
