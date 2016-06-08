#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

for I in Music/Commercial/*
do
    I_ESC=$(echo "$I" | "$BASE/esc.sh")
    [[ -d "$I" ]] || {
        echo "'$I_ESC' isn't a directory"
        continue
    }

    for ARTIST in "$I"/*
    do
        ARTIST_ESC=$(echo "$ARTIST" | "$BASE/esc.sh")
        [[ -d "$ARTIST" ]] || {
            echo "'$ARTIST_ESC' isn't a directory"
            continue
        }

        for ALBUM in "$ARTIST"/*
        do
            ALBUM_ESC=$(echo "$ALBUM" | "$BASE/esc.sh")
            [[ -d "$ALBUM" ]] || {
                echo "'$ALBUM_ESC' isn't a directory"
                continue
            }

            for TRACK in "$ALBUM"/*
            do
                TRACK_ESC=$(echo "$TRACK" | "$BASE/esc.sh")
                [[ -f "$TRACK" ]] || {
                    echo "'$TRACK_ESC' isn't a file"
                    continue
                }
            done
        done
    done
done
