#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function esc {
    sed -e "s/'/'\\\\''/g"
}

for INIT in Music/Commercial/*
do
    [[ -d "$INIT" ]] || {
        echo "Warning: '$INIT' is not a directory" >> /dev/stderr
        continue
    }
    for ARTIST in "$INIT"/*
    do
        [[ -d "$ARTIST" ]] || {
            echo "Warning: '$ARTIST' is not a directory" >> /dev/stderr
            continue
        }
        while read -r PAIR
        do
             FIRST=$(echo "$PAIR" | cut -f1 | esc)
            SECOND=$(echo "$PAIR" | cut -f2 | esc)

            echo "mv '$FIRST'/* '$SECOND'/"
        done < <(find "$ARTIST" -type d | "$BASE/list_dupe_guesses.sh")
    done
done
