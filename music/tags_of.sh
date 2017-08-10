#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

DIR=$(dirname  "$1")
F=$(basename "$1")

F_ESC=$(echo "$F" | sed -e 's/"/\"/g')

kid3-cli -c 'select "'"$F_ESC"'"' -c 'get all' "$DIR"
