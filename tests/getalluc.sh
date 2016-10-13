#!/usr/bin/env bash

function search {
    # Look for Big Buck Bunny, duplicate the output to stderr so we can see
    # progress
    BASE=$(dirname "$(dirname "$(readlink -f "$0")")")
    timeout 240 "$BASE/web/getalluc" big buck bunny | tee >(cat >&2)
}

MSG="Found video URL"
while read -r LINE
do
    if echo "$LINE" | grep "wget" > /dev/null
    then
        echo "ok - $MSG"
        exit 0
    fi
done < <(search)

echo "not ok - $MSG"
exit 1
