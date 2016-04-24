#!/usr/bin/env bash

set -e

function esc {
    sed -e "s@'@'\\''@g"
}

find Music/Commercial -name 'http___music.download.com' | while read -r DIR
do
    PARENT=$(dirname "$DIR")
    for ENTRY in "$DIR"/*
    do
         SRC=$(echo "$ENTRY"  | esc)
        DEST=$(echo "$PARENT" | esc)
        echo "mv '$SRC' '$DEST'"
    done
    ESCAPED=$(echo "$DIR" | esc)
    echo "rmdir '$ESCAPED'"
done

find Music/Commercial -name '*magnatune.com*'
