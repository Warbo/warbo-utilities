#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

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
                   D=$(dirname  "$F")
                   N=$(basename "$F" .oga)
               F_ESC=$(echo     "$F"     | "$BASE/esc.sh")
            OPUS_ESC=$(echo "$D/$N.opus" | "$BASE/esc.sh")
             OGG_ESC=$(echo "$D/$N.ogg"  | "$BASE/esc.sh")

            if file "$F" | grep -i opus > /dev/null
            then
                echo "'$F_ESC' should be renamed to .opus"
                echo "mv '$F_ESC' '$OPUS_ESC'"
            else if file "$F" | grep -i vorbis > /dev/null
                 then
                     echo "'$F_ESC' should be renamed to .ogg"
                     echo "mv '$F_ESC' '$OGG_ESC'"
                 else
                     echo "Unknown codec in '$F_ESC'"
                 fi
            fi
        done < <(find "$ARTIST" -iname "*.oga")

        while read -r F
        do
                   D=$(dirname  "$F")
                   N=$(basename "$F" .wav)
               F_ESC=$(echo     "$F"     | "$BASE/esc.sh")
            OPUS_ESC=$(echo "$D/$N.opus" | "$BASE/esc.sh")

            if file "$F" | grep "WAVE audio" > /dev/null
            then
                echo "'$F_ESC' can be encoded to .opus"
                echo "opusenc --bitrate 128 --comp 10 --max-delay 10 '$F_ESC' '$OPUS_ESC'"
            else
                echo "'$F_ESC' looks like Wave, but 'file' says:"
                file "$F"
                echo
                echo
            fi
        done < <(find "$ARTIST" -iname "*.wav")
    done
done
