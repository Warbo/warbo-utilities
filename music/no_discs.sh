#!/usr/bin/env bash

# Try to get rid of things like "(Disc 1)" in directory names, as they just
# lead to duplicates
for LETTER in Music/Commercial/*
do
    [[ -d "$LETTER" ]] || continue
    for ARTIST in "$LETTER"/*
    do
        for ALBUM in "$ARTIST"/*
        do
            [[ -d "$ALBUM" ]] || continue

            NAME=$(basename "$ALBUM")
            if echo "$NAME" | grep -i disc > /dev/null
            then
                echo "'$ALBUM' may be disc-specific album" >> /dev/stderr
            fi

            # Skip dodgy chars
            echo "$NAME" | rev > /dev/null 2> /dev/null || continue

            NODISC=$(echo "$NAME" | rev | cut -c 10- | rev)
            LOWER=$(echo "$NAME"     | tr '[:upper:]' '[:lower:]')
            NOLOWER=$(echo "$NODISC" | tr '[:upper:]' '[:lower:]')
            for DISC in 1 2 3 4 5
            do
                if [[ "x${NOLOWER} (disc ${DISC})" = "x$LOWER" ]]
                then
                    echo "Moving '$NAME' to '$NODISC'" >> /dev/stderr
                    DIR=$(dirname "$ALBUM")
                    pushd "$DIR" > /dev/null
                    mkdir -p "$NODISC"
                    for TRACK in "$NAME"/*
                    do
                        mv -v "$TRACK" "$NODISC"/
                    done
                    rmdir "$NAME"
                    popd > /dev/null
                fi
            done
        done
    done
done
