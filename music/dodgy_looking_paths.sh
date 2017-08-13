#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function withExtIn() {
    # Looks for files in $2 which end in extension $1; returns the paths but
    # strips off the extension (so we can stick a new one on)
    while read -r F
    do
        echo "$(dirname "$F")/$(basename "$F" ".$1")"
    done < <(find "$2" -iname "*.$1")
}

# Quick checks, where we know what to look for

for ARTIST in Music/Commercial/*/*
do
    NAME=$(basename "$ARTIST")

    if echo "$NAME" | grep '^.\.\(.\.\)*.$' > /dev/null
    then
        echo "'$NAME' should probably end in a '.'"
    fi

    if echo "$NAME" | grep ', The' > /dev/null
    then
        echo "'$NAME' should probably be 'The ...'"
    fi
done

find Music/Commercial/ -maxdepth 2 | while read -r P
do
    NAME=$(basename "$P")
    for ARTIST in Blue\ Oyster\ Cult  \
                      Blue\ Oeyster\ Cult \
                      Motvrhead           \
                      Motorhead           \
                      Motoerhead
    do
        if [[ "x$NAME" = "x$ARTIST" ]]
        then
            echo "Found badly named '$NAME' directory"
        fi
    done
done

# More thorough checks, for each file and directory
for I in Music/Commercial/*
do
    I_ESC=$(echo "$I" | "$BASE/esc.sh")
    [[ -d "$I" ]] || {
        echo "'$I_ESC' isn't a directory"
        continue
    }

    for ARTIST in "$I"/*
    do
        ARTIST_ESC=$(echo "$ARTIST" | "$BASE/esc.sh")
        [[ -d "$ARTIST" ]] || {
            echo "'$ARTIST_ESC' isn't a directory"
            continue
        }

        for ALBUM in "$ARTIST"/*
        do
            ALBUM_ESC=$(echo "$ALBUM" | "$BASE/esc.sh")
            [[ -d "$ALBUM" ]] || {
                echo "'$ALBUM_ESC' isn't a directory"
                continue
            }

            for TRACK in "$ALBUM"/*
            do
                TRACK_ESC=$(echo "$TRACK" | "$BASE/esc.sh")
                [[ -f "$TRACK" ]] || {
                    echo "'$TRACK_ESC' isn't a file"
                    continue
                }
            done
        done

        while read -r F
        do
            F_ESC=$(echo "$F" | "$BASE/esc.sh")
            echo "'$F_ESC' should probably be (losslessly) converted to ogg"
        done < <(find "$ARTIST" -iname "*.mka")

        while read -r F
        do
            [[ -e "$F.oga" ]] || {
                echo "No such file '$F.oga'; maybe rename to lower case?" 1>&2
                continue
            }

               F_ESC=$(echo "$F.oga"  | "$BASE/esc.sh")
            OPUS_ESC=$(echo "$F.opus" | "$BASE/esc.sh")
             OGG_ESC=$(echo "$F.ogg"  | "$BASE/esc.sh")

            if file "$F.oga" | grep -i opus > /dev/null
            then
                echo "'$F_ESC' should be renamed to .opus"
                echo "mv '$F_ESC' '$OPUS_ESC'"
            else if file "$F.oga" | grep -i vorbis > /dev/null
                 then
                     echo "'$F_ESC' should be renamed to .ogg"
                     echo "mv '$F_ESC' '$OGG_ESC'"
                 else
                     echo "Unknown codec in '$F_ESC'"
                 fi
            fi
        done < <(withExtIn "oga" "$ARTIST")

        while read -r F
        do
            [[ -e "$F.wav" ]] || {
                echo "Couldn't find '$F.wav'; rename to lowercase?" 1>&2
                continue
            }
               F_ESC=$(echo "$F.wav"  | "$BASE/esc.sh")
            OPUS_ESC=$(echo "$F.opus" | "$BASE/esc.sh")

            if file "$F.wav" | grep "WAVE audio" > /dev/null
            then
                echo "'$F_ESC' can be encoded to .opus"
                echo "opusenc --bitrate 128 --comp 10 --max-delay 10 '$F_ESC' '$OPUS_ESC'"
            else
                echo "'$F_ESC' looks like Wave, but 'file' says:"
                file "$F.wav"
                echo
                echo
            fi
        done < <(withExtIn "wav" "$ARTIST")

        for EXT in mp4 avi
        do
            while read -r F
            do
                F_ESC=$(echo "$F.$EXT" | "$BASE/esc.sh")
                echo "Found possible video file '$F_ESC'"
            done < <(withExtIn "$EXT" "$ARTIST")
        done
    done
done
