#!/usr/bin/env bash

set -e

function esc {
    sed -e "s@'@'\\\\''@g"
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

find Music/Commercial -type d -name '*magnatune.com*' | while read -r DIR
do
     SRC=$(echo "$DIR" | esc)
    DEST=$(echo "$DIR" | sed -e 's@ (PREVIEW_ buy it at www.magnatune.com)@@g' | esc)
    echo "mv -v '$SRC' '$DEST'"
done

find Music/Commercial -type f -name '*magnatune.com*' | while read -r F
do
     SRC=$(echo "$F" | esc)
    DEST=$(echo "$F" | sed -e 's@ (PREVIEW_ buy it at www.magnatune.com)@@g' | esc)
    echo "mv -v '$SRC' '$DEST'"
done

find Music/Commercial -type f -name 'tmp_*' | while read -r F
do
    echo "Name '$F' looks like a dupe" 1>&2
    OTHER=$(basename "$F" | cut -d '_' -f 2-)
      DIR=$(dirname "$F")
      ESC=$(echo "$F" | esc)
    if [[ -e "$DIR/$OTHER" ]]
    then
        echo "rm -v '$ESC'"
    else
        echo "No equivalent '$OTHER' found in '$DIR' though" 1>&2
    fi
done
