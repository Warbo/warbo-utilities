#!/usr/bin/env bash

BASE=$(dirname "$(dirname "$(readlink -f "$0")")")

MSG="Found video URL"

if echo "'$BASE/web/getalluc' big buck bunny host:vidzi.tv" |
   timeout 240 bash | grep -m 1 "wget"
then
    echo "ok - $MSG"
    exit 0
fi

echo "not ok - $MSG"
exit 1
