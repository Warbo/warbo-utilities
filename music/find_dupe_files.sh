#!/usr/bin/env bash
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
