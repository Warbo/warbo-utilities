#!/usr/bin/env bash

set -e

function esc {
    sed -e "s@'@'\\\\''@g"
}

printf 'Checking stdin for files with dodgy names' 1>&2
while read -r NAME
do
    printf '.' 1>&2
    case "$NAME" in
        *http___music.download.com*)
            PARENT=$(dirname "$NAME")
            for ENTRY in "$DIR"/*
            do
                 SRC=$(echo "$ENTRY"  | esc)
                DEST=$(echo "$PARENT" | esc)
                echo "mv '$SRC' '$DEST'"
            done
            ESCAPED=$(echo "$NAME" | esc)
            echo "rmdir '$ESCAPED'"
            ;;

        *magnatune.com*)
             SRC=$(echo "$NAME" | esc)
            DEST=$(echo "$NAME" |
                   sed -e 's@ (PREVIEW_ buy it at www.magnatune.com)@@g' | esc)
            echo "mv -v '$SRC' '$DEST'"
            ;;

        *tmp_*)
            [[ -f "$NAME" ]] || continue
            echo -e "\nName '$NAME' looks like a dupe" 1>&2
            OTHER=$(basename "$NAME" | cut -d '_' -f 2-)
              DIR=$(dirname "$NAME")
              ESC=$(echo "$NAME" | esc)
              if [[ -e "$DIR/$OTHER" ]]
              then
                  echo "rm -v '$ESC'"
              else
                  echo -e "\nNo equivalent '$OTHER' found in '$DIR' though" 1>&2
              fi
              ;;
    esac
done
