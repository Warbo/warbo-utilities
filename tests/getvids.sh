#!/usr/bin/env bash

function get {
  BASE=$(dirname "$(dirname "$(readlink -f "$0")")")
  export SITE="http://www.alluc.ee"
  "$BASE/web/getvids" big buck bunny
}

function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
        return 0
    else
        echo "not ok - $2"
        ERR=1
        return 1
    fi
}

ERR=0

FOUND=0
while read -r LINE
do
    echo "$LINE" | grep "Downloading" > /dev/null && FOUND=1 && break
done < <(get 2>&1)

[[ "$FOUND" -eq 1 ]]
report "$?" "Got URL"

FOUND=0
while read -r LINE
do
    echo "$LINE" | grep "Getting vids from page" > /dev/null && FOUND=1 && break
done < <(get 2>&1)

[[ "$FOUND" -eq 1 ]]
report "$?" "Found results"

exit "$ERR"
