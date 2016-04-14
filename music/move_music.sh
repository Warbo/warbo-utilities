#!/bin/bash
shopt -s nullglob

BASE=$(dirname "$(readlink -f "$0")")

function find_dupe_artists {
    # For each directory at the artist level, we 'simplify' the name to see if
    # it collides with another, eg. "AC-DC" and "ACDC" both become "acdc"
    echo "Looking for possible dupes in Music/Commercial"

    for INIT in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    do
        echo "$INIT"
        "$BASE/guess_dupes.sh" < <(ls "Music/Commercial/$INIT/")
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
            "$BASE/guess_dupes.sh" < <(find "$ARTIST" -type d)
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
            DUPES=$("$BASE/guess_dupes.sh" < <(find "$ARTIST" -type f))
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
                    echo "COMPARE	$TRACK	$NAME"
                done < <(echo "$AFTER" | head -n "$END"        |
                                         grep -v "looks like:" |
                                         grep -v "^END$")
            done < <(echo "$DUPES" | grep -n "looks like")
        done
    done
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

#"$BASE/move_contents.sh"
#"$BASE/delete_crap.sh"
#"$BASE/remove_empties.sh"
#find_dupe_artists
find_dupe_files
#fdupes_per_artist
#"$BASE/no_discs.sh"
#"$BASE/free_out_of_commercial.sh"
#"$BASE/normalise_whitespace.sh"
