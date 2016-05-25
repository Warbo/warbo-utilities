#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function normalise {
    "$BASE/strip_name.sh" "$1"
}

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
