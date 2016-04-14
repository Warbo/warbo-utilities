#!/bin/bash
shopt -s nullglob

# Cache calculated CRCs
if [[ -e .crcs ]]
then
    echo "Found .crcs" >> /dev/stderr
    CRCMAP=$(cat .crcs)
else
    echo "No .crcs found, making one" >> /dev/stderr
    touch .crcs
    CRCMAP=""
fi

function get_crc {
    while read -r LINE
    do
        LN=$(echo "$LINE" | cut -f 2-)
        [[ "x$LN" = "x$1" ]] && {
            echo "$LINE" | cut -f 1
            return 0
        }
    done < <(echo "$CRCMAP")
    CRC=$(avconv -i "$1" -f crc - 2> /dev/null | grep CRC)
    echo -e "$CRC\t$1" >> .crcs
    CRCMAP=$(echo "$CRCMAP"; echo -e "$CRC\t$1")
    echo "$CRC"
}

function compare_files {
    [[ -e "$1" ]] || {
        echo "Can't compare non-existent '$1' to '$2'" >> /dev/stderr
        return 1
    }
    [[ -e "$2" ]] || {
        echo "Can't compare '$1' to non-existent '$2'" >> /dev/stderr
        return 1
    }
    IS_AUDIO=0
    LOWER1=$(echo "$1" | tr '[:upper:]' '[:lower:]')
    LOWER2=$(echo "$2" | tr '[:upper:]' '[:lower:]')

    for EXT in mp3 wma aac ogg m4a
    do
        if [[ "$LOWER1" =~ "$EXT"$ ]] && [[ "$LOWER2" =~ "$EXT"$ ]]
        then
            IS_AUDIO=1
        fi
    done

    if [[ "$IS_AUDIO" -eq 1 ]]
    then
        SRC=$(get_crc "$1")
        DST=$(get_crc "$2")
        if [[ "x$SRC" = "x$DST" ]]
        then
            echo "$1 is a duplicate of $2"
            if [[ -d "DUPES" ]]
            then
                D=$(dirname "$2")
                mkdir -p "DUPES/$D"
                mv -v "$2" "DUPES/$D/"
            fi
        else
            echo "$1 doesn't match CRC of $2"
        fi
    else
        echo "Path $1 looks like a dupe of $2"
    fi
}

function move_if_no_conflict {
    # Takes the initial (subdir of Music/Commercial), the directory we might be
    # moving from and a path within that directory. For example:
    #
    # move_if_no_conflict "A" "MyMusic" "Ayreon/Into the Electric Castle"
    #
    # If "Music/Commercial/A/Ayreon/Into the Electric Castle" does not exist,
    # then "MyMusic/Ayreon/Into the Electric Castle" will be moved there.
    #
    # If it does exist, and both are directories, then move_if_no_conflict will
    # be called recursively on all of the contents (using the same initial and
    # source, ie. "A" and "MyMusic" in this example)
    #
    # If it does exist, is not a directory, and the path appears to be an audio
    # file (eg. ending in "mp3"), then we take the CRC checksum of both audio
    # streams and, if they match, report the duplicate for deletion.
    INITIAL="$1"
    SOURCE="$2"
    THEPATH="$3"
    if [ -e "Music/Commercial/$INITIAL/$THEPATH" ]
    then
        if [ -d "Music/Commercial/$INITIAL/$THEPATH" ]
        then
            if [ -d "$SOURCE/$THEPATH" ]
            then
                for INNER in "$SOURCE/$THEPATH"/*
                do
                    RELATIVE=$(echo "$INNER" | sed -e 's@^[^/]*/@@')
                    move_if_no_conflict "$INITIAL" "$SOURCE" "$RELATIVE"
                done
            else
                echo "Directory/non-directory mixup for $THEPATH"
            fi
        else
            compare_files "$SOURCE/$THEPATH" "Music/Commercial/$INITIAL/$THEPATH"
        fi
    else
        mv -nv "$SOURCE/$THEPATH" "Music/Commercial/$INITIAL/$THEPATH"
    fi
}

function move_contents {
    echo "Moving non-conflicting artist contents"
    for INIT in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    do
        echo "$INIT"
        for COLLECTION in "LozMusic" "LozMusic2" "Jo Tidy Music" "JamesMusic" "Riffs"
        do
            for DIR in "$COLLECTION/$INIT"*
            do
                RELATIVE=$(echo "$DIR" | sed -e 's@^[^/]*/@@')
                move_if_no_conflict "$INIT" "$COLLECTION" "$RELATIVE"
            done
        done
    done
}

function remove_empties {
    echo "Removing empty directories in Music/"
    find "Music" -type d -exec rmdir --ignore-fail-on-non-empty {} \;
}

function normalise {
    echo "$1"                  |
    tr '[:upper:]' '[:lower:]' |
    tr -cd '[:lower:]'
}

function guess_dupes {
    # \n-separated list of names and their simplified alternatives
    NAMES=""
    while read -r INCOMING
    do
        NAME=$(basename "$INCOMING")

        # Upper -> lower, remove non-alphabetic
        ALT=$(normalise "$NAME")

        if DUPE=$(echo "$NAMES" | grep "$ALT")
        then
            echo "$INCOMING looks like:"
            echo "$DUPE" | grep -o "DIR:.*     " |
                grep -o ": .*" |
                grep -o " .*"  |
                grep -o "[^ ].*"
            echo "END"
        fi

        NAMES="$NAMES
DIR: $INCOMING     ALT: $ALT"
    done
}

function find_dupe_artists {
    # For each directory at the artist level, we 'simplify' the name to see if
    # it collides with another, eg. "AC-DC" and "ACDC" both become "acdc"
    echo "Looking for possible dupes in Music/Commercial"

    for INIT in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    do
        echo "$INIT"
        guess_dupes < <(ls "Music/Commercial/$INIT/")
    done
}

function find_dupe_dirs {
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
            guess_dupes < <(find "$ARTIST" -type d)
        done
    done
}

function find_dupe_files {
    # Look for similar filenames inside each artist directory
    for INIT in Music/Commercial/*
    do
        [[ -d "$INIT" ]] || continue
        for ARTIST in "$INIT"/*
        do
            [[ -d "$ARTIST" ]] || continue
            echo "Looking for dupes in '$ARTIST'" >> /dev/stderr
            DUPES=$(guess_dupes < <(find "$ARTIST" -type f))
            echo "Possible dupes:" >> /dev/stderr
            echo "$DUPES"          >> /dev/stderr
            echo "Checking CRCs"   >> /dev/stderr
            while read -r LINE
            do
                NUM=$(echo "$LINE" | cut -d ':' -f1)
                AFTER=$(echo "$DUPES" | tail -n +"$NUM")
                END=$(echo "$AFTER" | grep -n "^END$" | cut -d ':' -f1 | head -n1)
                TRACK=$(echo "$LINE" | sed -e 's/ looks like://g' | cut -d ':' -f 2-)
                while read -r NAME
                do
                    compare_files "$TRACK" "$NAME"
                done < <(echo "$AFTER" | head -n "$END"        |
                                         grep -v "looks like:" |
                                         grep -v "^END$")
            done < <(echo "$DUPES" | grep -n "looks like")
        done
    done
}

function delete_crap {
    echo "Looking for crap you might want to delete"
    find "Music" -iname "*.db"   \
             -or -iname "*.jpg"  \
             -or -iname "*.jpeg" \
             -or -iname "*.url"  \
             -or -iname "*.txt"  \
             -or -iname "*.ini"
}

function fdupes_per_artist {
    for LETTER in Music/*
    do
        [[ -d "$LETTER" ]] || continue
        for ARTIST in "$LETTER"/*
        do
            [[ -d "$ARTIST" ]] || continue
            fdupes -d -r "$ARTIST"
        done
    done
}

function no_discs {
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
}

function free_dirs {
    # Directories which should be in Free rather than Commercial
    for D in "N/Nanowar" "W/Wenlock" "Z/ZX Spectrum" "C/Chiptunes"
    do
        [[ -d "$D" ]] && echo "$D"
    done
    find . -type d -name 'Final Fantasy VII*'
    find . -type d -name 'Newgrounds Audio Portal*'
    find . -type d -name 'Sonic the Hedgehog 2*'
}

function free_out_of_commercial {
    # Move directories which should be in Free out of Commercial
    # (Mostly to appease Jo)
    pushd Music/Commercial > /dev/null
    mkdir -p ../Free
    while read -r DIR
    do
        PARENT=$(dirname "$DIR")
        mkdir -p ../Free/"$PARENT"
        echo "Moving '$DIR' to ../Free/$DIR"
        mv -v "$DIR" ../Free/"$DIR"
    done < <(free_dirs)
    popd > /dev/null
}

function normalise_whitespace {
    # Look for dodgy whitespace in filenames, which is either ugly or may lead
    # to dupes. Includes 'double  spaces', ' initial spaces' and
    # 'spaces before .extensions'
    while read -r NAME
    do
        [[ -d "$NAME" ]] && {
            echo "'$NAME' has weird whitespace" >> /dev/stderr
            continue
        }
        [[ -f "$NAME" ]] && {
            DIR=$(dirname "$NAME")
            FILE=$(basename "$NAME")
            NORMAL=$(echo "$FILE" | sed -e 's/   */ /g')
            echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
        }
    done < <(find Music -name '*  *')
    while read -r NAME
    do
        [[ -d "$NAME" ]] && {
            echo "'$NAME' has weird whitespace" >> /dev/stderr
            continue
        }
        [[ -f "$NAME" ]] && {
            DIR=$(dirname "$NAME")
            FILE=$(basename "$NAME")
            NORMAL=$(echo "$FILE" | sed -e 's/^  *//g')
            echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
        }
    done < <(find Music -name ' *')
    while read -r NAME
    do
        [[ -d "$NAME" ]] && {
            echo "'$NAME' has weird whitespace" >> /dev/stderr
            continue
        }
        [[ -f "$NAME" ]] && {
            DIR=$(dirname "$NAME")
            FILE=$(basename "$NAME")
            NORMAL=$(echo "$FILE" | sed -e 's/  *\.\([^\.]*\)$/\.\1/g')
            [[ "x$FILE" = "x$NORMAL" ]] ||
                echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
        }
    done < <(find Music -name '* .*')
}

#move_contents
#delete_crap
#remove_empties
#find_dupe_artists
find_dupe_files
#fdupes_per_artist
#no_discs
#free_out_of_commercial
#normalise_whitespace
