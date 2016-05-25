#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

function normalise {
    "$BASE/strip_name.sh" "$1"
}

function printDupes {
    NAMES=""
    while read -r INCOMING
    do
        NAME=$(basename  "$INCOMING")
         ALT=$(normalise "$NAME")

        # Print out duplicates
        echo "$NAMES" | grep "$ALT" | cut -f 1 | while read -r DUPE
        do
            printf "%s\t%s\n" "$NAME" "$DUPE"
        done

        if [[ -z "$NAMES" ]]
        then
            NAMES=$(printf "%s\t%s"            "$NAME" "$ALT")
        else
            NAMES=$(printf "%s\n%s\t" "$NAMES" "$NAME" "$ALT")
        fi
    done
}

if [[ $(( RANDOM % 2 )) -eq 0 ]]
then
    printDupes
else
    tac | printDupes
fi
