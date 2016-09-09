#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

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
                shopt -s nullglob
                for INNER in "$SOURCE/$THEPATH"/*
                do
                    RELATIVE=$(echo "$INNER" | sed -e 's@^[^/]*/@@')
                    move_if_no_conflict "$INITIAL" "$SOURCE" "$RELATIVE"
                done
            else
                echo "Directory/non-directory mixup for $THEPATH"
            fi
        else
            echo "COMPARE	$SOURCE/$THEPATH	Music/Commercial/$INITIAL/$THEPATH"
        fi
    else
        OUT_S=$(echo "$SOURCE/$THEPATH"                   | "$BASE/esc.sh")
        OUT_D=$(echo "Music/Commercial/$INITIAL/$THEPATH" | "$BASE/esc.sh")
        echo "mv -nv '$OUT_S' '$OUT_D'"
    fi
}

function move_contents {
    echo "Moving non-conflicting artist contents"
    for INIT in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    do
        echo "$INIT"
        for COLLECTION in "LozMusic" "LozMusic2" "Jo Tidy Music" "JamesMusic" "Riffs" "ChrisLaptopMusic"
        do
            shopt -s nullglob
            for DIR in "$COLLECTION/$INIT"*
            do
                RELATIVE=$(echo "$DIR" | sed -e 's@^[^/]*/@@')
                move_if_no_conflict "$INIT" "$COLLECTION" "$RELATIVE"
            done
        done
    done
}

move_contents
