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
        TRACK_DIR=".artist_name_cache/$INIT/${NAME}_${CNT}.tracks"

        if [[ -f "$ALBUM_CACHE" ]] && [[ -d "$TRACK_DIR" ]]
        then
            while read -r NAME_URL
            do
                ALBUM=$(echo "$NAME_URL" | cut -f1)
                ALBUM_DIR="$ARTIST_DIR/$ALBUM"
                [[ -d "$ALBUM_DIR" ]] || continue

                URL=$(echo "$NAME_URL" | cut -f2)

                TRACK_BASE=$(echo "$URL" | tr -c '[[:alnum:]]' '_')
                TRACK_FILE="$TRACK_DIR/$TRACK_BASE"

                [[ -f "$TRACK_FILE" ]] || continue

                while read -r NUM_TRACK
                do
                    RAW_NUM=$(echo "$NUM_TRACK" | cut -f1 | tr -dc '[[:digit:]]')
                    NUM=$(printf "%02d" "$RAW_NUM")
                    TRACK=$(echo "$NUM_TRACK" | cut -f2)

                    FOUND=0
                    FILES=""
                    for F in "$ALBUM_DIR"/*
                    do
                        F_BASE=$(basename "$F")
                        if echo "$F_BASE" | grep -F "$TRACK" > /dev/null
                        then
                            FOUND=$(( FOUND + 1 ))
                            FILES=$(echo -e "$FILES\n$F")
                        elif echo "$TRACK" | grep -F "$F_BASE" > /dev/null
                        then
                            FOUND=$(( FOUND + 1 ))
                            FILES=$(echo -e "$FILES\n$F")
                        fi
                    done

                    if [[ "$FOUND" -eq 0 ]]
                    then
                        echo "Couldn't find track '$ALBUM_DIR/$NUM - $TRACK'" 1>&2
                    elif [[ "$FOUND" -gt 1 ]]
                    then
                        echo "Found multiple matches for '$ALBUM_DIR/$NUM - $TRACK'" 1>&2
                        echo "$FILES" | grep '^.' 1>&2
                    fi
                done < <("$BASE/tracks_from.sh" < "$TRACK_FILE")
            done < <("$BASE/album_names_from.sh" < "$ALBUM_CACHE")
        fi
    done
done
