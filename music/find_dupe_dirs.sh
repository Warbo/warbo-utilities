#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

for INIT in Music/Commercial/*
do
    [[ -d "$INIT" ]] || {
        echo "Warning: '$INIT' is not a directory" >> /dev/stderr
        continue
    }
    for ARTISTS in "$INIT"/*
    do
        [[ -d "$ARTIST" ]] || {
            echo "Warning: '$ARTIST' is not a directory" >> /dev/stderr
            continue
        }
        find "$ARTIST" -type d | "$BASE/guess_dupes.sh"
    done
done
