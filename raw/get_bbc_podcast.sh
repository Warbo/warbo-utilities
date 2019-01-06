#!/usr/bin/env bash

URL=$(wget -q -O- "$1" | xidel -q -e '//a/@href' - |
                         grep '\.mp3'              |
                         grep -v -- '-low'         |
                         head -n1)

# Sanity check
echo "$URL" | grep '^//' > /dev/null && {
    echo "Prepending 'http:' to '$URL'" 1>&2
    URL="http:$URL"
}

echo "$URL" | grep '^http' > /dev/null || {
    echo "While fetching '$1', programme URL '$URL' not http, aborting" 1>&2
    exit 1
}

echo "$URL" | grep 'mp3$' > /dev/null || {
    echo "While fetching '$1', programme URL '$URL' not an mp3, aborting" 1>&2
    exit 1
}

wget "$URL"
