#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function esc {
    "$BASE/esc.sh"
}

function same {
    X=$(echo "$1" | tr -dc '[:alnum:]' | tr '[:upper:]' '[:lower:]')
    Y=$(echo "$2" | tr -dc '[:alnum:]' | tr '[:upper:]' '[:lower:]')
    if [[ "x$X" = "x$Y" ]]
    then
        return 0
    fi
    return 1
}

TOP=$(readlink -f "Music/Commercial")

function processDir {
    DIR=$(readlink -f "$1")
    echo "$DIR" | grep "^$TOP/" > /dev/null || {
        echo "Given dir '$DIR' doesn't begin with '$TOP'" 1>&2
        exit 1
    }

      BITS=$(echo "$DIR"  | sed  -e "s@^$TOP/@@g" | tr '/' '\n')
    ARTIST=$(echo "$BITS" | head -n2 | tail -n1)

    [[ -n "$INIT" ]] || INIT=$(echo "$ARTIST" | cut -c1)

    [[ -d "$TOP/$INIT" ]] || {
        echo "No dir '$TOP/$INIT'" 1>&2
        exit 1
    }

    [[ -d "$TOP/$INIT/$ARTIST" ]] || {
        echo "No dir '$TOP/$INIT/$ARTIST'" 1>&2
        exit 1
    }

    while read -r F
    do
        ALBUM=""
        TAGS=$("$BASE/tags_of.sh" "$F")
        if echo "$TAGS" | grep "^  Album  " > /dev/null
        then
            ALBUM=$("$BASE/tags_of.sh" "$F" | grep '^  Album  '  |
                                              sed -e 's/  */ /g' |
                                              cut -d ' ' -f3-    | head -n1)
        fi
        [[ -n "$ALBUM" ]] || continue

        D=$(dirname "$(readlink -f "$F")")
        same "$D" "$TOP/$INIT/$ARTIST/$ALBUM" || {
            EF=$(echo "$F"                        | esc)
            EA=$(echo "$ARTIST"                   | esc)
            EB=$(echo "$ALBUM"                    | esc)
            EP=$(echo "$TOP/$INIT/$ARTIST/$ALBUM" | esc)

            echo "File '$F' has album '$ALBUM'; move with command:"
            echo "mkdir -p '$EP'"
            echo "mv '$EF' '$EP/'"
        }
    done < <(find "$DIR" -type f)
}

if [[ -n "$1" ]]
then
    processDir "$1"
else
    for I in Music/Commercial/*
    do
        [[ -d "$I" ]] || continue
        INIT=$(basename "$I")
        for A in "$I"/*
        do
            [[ -d "$A" ]] || continue
            processDir "$A"
        done
    done
fi
