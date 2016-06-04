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
            # Extract all album names
            ALBUMS=$(xidel -q - -e '//td/a[@class="album"]' < "$ALBUM_CACHE")

            # We want to be as fuzzy as possible in our matching, but we need to
            # keep track of what's already been used. If we have an exact match
            # for the album "Foo", we don't want to suggest that directory as a
            # match for "Foo Part II: The Second Chapter".

            # Build a list of directories which aren't exact matches, and albums
            # which haven't been found
            DIRS=""
            UNSEEN="$ALBUMS"
            for D in "$ARTIST_DIR"/*
            do
                # Skip exact matches, and remove them from UNSEEN
                D_BASE=$(basename "$D")
                if echo "$ALBUMS" | grep -Fx -- "$D_BASE" > /dev/null
                then
                    UNSEEN=$(echo "$UNSEEN" | grep -vFx -- "$D_BASE")

                    # Report if the match isn't a directory
                    [[ -d "$D" ]] ||
                        echo "ERROR: '$D' is a file, not a directory!" 1>&2

                    continue
                fi

                # Skip non-directories
                [[ -d "$D" ]] || continue

                # Otherwise, D seems worth fuzzy-matching
                DIRS=$(echo -e "$DIRS\n$D" | grep '^.')
            done

            # For each unseen album, look for fuzzy matches
            while read -r ALBUM
            do
                [[ -n "$ALBUM" ]] || continue

                ALBUM_STRIP=$("$BASE/strip_name.sh" "$ALBUM")

                CHARS=$(echo -n "$ALBUM_STRIP" | wc -c)
                if [[ "$CHARS" -lt 4 ]]
                then
                    echo "No exact match for '$ALBUM' in '$ARTIST_DIR', and unsuitable for fuzzy matching" 1>&2
                    continue
                fi

                ALBUM_NAME_ESC=$(echo "$ALBUM" | sed -e "s/'/'\\\\''/g")
                ALBUM_NOSLASH=$(echo "$ALBUM"  | sed -e "s@/@_@g")
                ALBUM_ESC=$(echo "$ARTIST_DIR/$ALBUM_NOSLASH" | sed -e "s/'/'\\\\''/g")

                FOUND=0
                while read -r D
                do
                    [[ -n "$D" ]] || continue

                    D_DIR=$(dirname "$D")
                    D_BASE=$(basename "$D")
                    D_STRIP=$("$BASE/strip_name.sh" "$D_BASE")

                    CHARS=$(echo -n "$D_STRIP" | wc -c)

                    if [[ "$CHARS" -lt 4 ]]
                    then
                        echo "Skipping directory '$D' as it's unsuitable for fuzzy matching" 1>&2
                        continue
                    fi

                    D_ESC=$(echo "$D" | sed -e "s/'/'\\\\''/g")

                    if echo "$ALBUM_STRIP" | grep -F -- "$D_STRIP" > /dev/null
                    then
                        FOUND=1
                        echo "Directory '$D' looks like album '$ALBUM'. To rename, do:" 1>&2
                        echo "mv '$D_ESC' '$ALBUM_ESC'" 1>&2
                        break
                    fi

                    if echo "$D_STRIP" | grep -F -- "$ALBUM_STRIP" > /dev/null
                    then
                        FOUND=1
                        echo "Directory '$D' looks like album '$ALBUM'. To rename, do:" 1>&2
                        echo "mv '$D_ESC' '$ALBUM_ESC'" 1>&2
                        break
                    fi
                done < <(echo "$DIRS")

                if [[ "$FOUND" -eq 0 ]]
                then
                    if [[ -n "$CNT" ]]
                    then
                        echo "Couldn't find album '$ALBUM_NAME_ESC' by $NAME ($CNT)"
                    else
                        echo "Couldn't find album '$ALBUM_NAME_ESC' by $NAME"
                    fi
                fi
            done < <(echo "$UNSEEN")
        fi
    done
done
