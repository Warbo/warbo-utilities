#!/usr/bin/env bash

command -v xidel > /dev/null || {
    echo "xidel not found" 1>&2
    exit 1
}

BASE=$(dirname "$(readlink -f "$0")")

for INIT_DIR in Music/Commercial/*
do
    [[ -d "$INIT_DIR" ]] || continue
    INIT=$(basename "$INIT_DIR" | cut -c1)

    for ARTIST_DIR in "$INIT_DIR"/*
    do
        [[ -d "$ARTIST_DIR" ]] || continue
        DIR_NAME=$(basename "$ARTIST_DIR")
        NAME_COUNTRY=$("$BASE/dir_to_artist_country.sh" "$DIR_NAME")

        NAME=$(echo "$NAME_COUNTRY" | cut -f1)
         CNT=$(echo "$NAME_COUNTRY" | cut -f2)

        ALBUM_CACHE=".artist_name_cache/$INIT/${NAME}_${CNT}.albums"

        if [[ -f "$ALBUM_CACHE" ]]
        then
            xidel -q - -e '//td/a[@class="album"]' < "$ALBUM_CACHE" |
                while read -r ALBUM
                do
                    ALBUM_STRIP=$("$BASE/strip_name.sh" "$ALBUM")
                    FOUND=0
                    for F in "$ARTIST_DIR"/*
                    do
                        F_STRIP=$("$BASE/strip_name.sh" "$(basename "$F")")

                        if [[ "x$ALBUM_STRIP" = "x$F_STRIP" ]]
                        then
                            FOUND=1
                            break
                        fi
                    done

                    if [[ "$FOUND" -eq 0 ]]
                    then
                        if [[ -n "$CNT" ]]
                        then
                            echo "Couldn't find album '$ALBUM' by $NAME ($CNT)"
                        else
                            echo "Couldn't find album '$ALBUM' by $NAME"
                        fi
                    fi
                done
        fi
    done
done
