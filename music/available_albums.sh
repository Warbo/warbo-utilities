#!/usr/bin/env bash

command -v xidel > /dev/null || {
    echo "xidel not found" 1>&2
    exit 1
}

BASE=$(dirname "$(readlink -f "$0")")

for INIT_DIR in Music/Commercial/*
do
    [[ -d "$INIT_DIR" ]] || continue
    INIT=$(basename "$INIT_DIR")

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
                    if [[ -d "$ARTIST_DIR/$ALBUM" ]]
                    then
                        FOUND=1
                    elif [[ -e "$ARTIST_DIR/$ALBUM" ]]
                    then
                        echo "ERROR: '$ARTIST_DIR/$ALBUM' is a file, not a directory!" 1>&2
                        FOUND=1
                    else
                        for F in "$ARTIST_DIR"/*
                        do
                            [[ -d "$F" ]] || continue

                            F_DIR=$(dirname "$F")
                            F_BASE=$(basename "$F")
                            F_STRIP=$("$BASE/strip_name.sh" "$F_BASE")

                            F_ESC=$(echo "$F" | sed -e "s/'/'\\\\''/g")
                            ALBUM_NOSLASH=$(echo "$ALBUM" | sed -e "s@/@_@g")
                            ALBUM_ESC=$(echo "$ARTIST_DIR/$ALBUM_NOSLASH" | sed -e "s/'/'\\\\''/g")

                            if [[ "x$ALBUM" = "x$F_BASE" ]]
                            then
                                FOUND=1
                                break
                            fi

                            if echo "$ALBUM_STRIP" | grep -F "$F_STRIP" > /dev/null
                            then
                                FOUND=1
                                echo "Directory '$F' looks like album '$ALBUM'. To rename, do:" 1>&2
                                echo "mv '$F_ESC' '$ALBUM_ESC'" 1>&2
                                break
                            fi

                            if echo "$F_STRIP" | grep -F "$ALBUM_STRIP" > /dev/null
                            then
                                FOUND=1
                                echo "Directory '$F' looks like album '$ALBUM'. To rename, do:" 1>&2
                                echo "mv '$F_ESC' '$ALBUM_ESC'" 1>&2
                                break
                            fi
                        done
                    fi

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
