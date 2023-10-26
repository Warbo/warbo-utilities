#!/usr/bin/env bash
set -e

CONTENT=$(wget -q -O- "$2")

function process {
    echo "$CONTENT" | grep -io "$1\\W0-9[0-9]*"  | tr '[:upper:]' '[:lower:]'
}

SERIES=$(process "series")
SEASON=$(process "season")
EPISODE=$(process "episode")

S=""
while read -r LINE
do
    S="$LINE"
done < <(printf '%s\n%s' "$SERIES" "$SEASON" | grep '^.')

E=""
while read -r LINE
do
    E="$LINE"
done < <(echo "$EPISODE" | grep '^.')

[[ -n "$S" ]] || {
    echo "No series found for '$1'" 1>&2
    exit 1
}

[[ -n "$E" ]] || {
    echo "No episode found for '$1'" 1>&2
    exit 1
}

echo "$S $E"
