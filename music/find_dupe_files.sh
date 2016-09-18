#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

echo "Looking for transcoded files" 1>&2
FORMATS="ogg mp3 opus wma flac"
for FMT1 in $FORMATS
do
    for FMT2 in $FORMATS
    do
        find Music -type f -iname "*.$FMT1.$FMT2" | while read -r F
        do
            ORIG=$(basename "$F" ".$FMT2")
            if [[ -e "$ORIG" ]]
            then
                echo "'$F' seems to be transcoded from '$ORIG'"
            else
                echo "'$F' seems to be transcoded; rename?"
            fi
        done
    done
done

# Look for similar filenames inside each artist directory
for INIT in Music/Commercial/*
do
    [[ -d "$INIT" ]] || continue
    for ARTIST in "$INIT"/*
    do
        [[ -d "$ARTIST" ]] || continue
        echo "Looking for dupes in '$ARTIST'" >> /dev/stderr
        DUPES=$(find "$ARTIST" -type f | "$BASE/guess_dupes.sh")
        echo "Possible dupes:" >> /dev/stderr
        echo "$DUPES"
        echo "Checking CRCs"   >> /dev/stderr
        echo "$DUPES" | grep -n "looks like" | while read -r LINE
        do
            NUM=$(echo "$LINE" | cut -d ':' -f1)
            AFTER=$(echo "$DUPES" | tail -n +"$NUM")
            END=$(echo "$AFTER" | grep -n "^END$" | cut -d ':' -f1 | head -n1)
            TRACK=$(echo "$LINE" | sed -e 's/ looks like://g' | cut -d ':' -f 2-)
            echo "$AFTER" | head -n "$END"        |
                            grep -v "looks like:" |
                            grep -v "^END$"       | while read -r NAME
            do
                echo "COMPARE	$TRACK	$NAME"
            done
        done
    done
done | "$BASE/compare_crcs.py"
