#!/usr/bin/env bash

BASE=$(dirname "$(dirname "$(readlink -f "$0")")")

MSG="Found video URL"

if STOPONFIRST=1 "$BASE/web/getalluc" big buck bunny host:vidzi.tv | grep -m 1 "wget"
then
    echo "ok - $MSG"
    exit 0
fi

echo "not ok - $MSG"
exit 1
