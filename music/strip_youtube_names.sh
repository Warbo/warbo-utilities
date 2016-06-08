#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

while read -r F
do
    STRIPPED=$(echo "$F" | sed -e 's/-[^ ()]*\.\(....*\)/.\1/g')

           F_ESC=$(echo "$F"        | "$BASE/esc.sh")
    STRIPPED_ESC=$(echo "$STRIPPED" | "$BASE/esc.sh")

    echo "mv '$F_ESC' '$STRIPPED_ESC'"
done < <(find Music/Commercial -type f | grep -- '-[^ ]*\....[.]*')
