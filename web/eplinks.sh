#!/usr/bin/env bash

if [[ "$#" -lt 1 ]]
then
    echo "Reading links from stdin" 1>&2
    LINKS=$(cat)
else
    echo "Fetching links from '$1'" 1>&2
    PAGE=$(curl -s "$1")

    LINKS=$(echo "$PAGE" |
            xidel -q -e '//div[@class="site"]/a/@data-actuallink' -)

    [[ -n "$TITLE" ]] ||
        TITLE=$(echo "$PAGE" | xidel -q -e '//h1' -          |
                               sed -e 's/ - Watch Online//g' |
                               grep '^.'                     |
                               head -n1)
fi

[[ -n "$TITLE" ]] || TITLE="UNKNOWN"
TITLE=$(echo "$TITLE" | tr '/:\t' '_')

echo "$LINKS" | while read -r LINK
do
    echo "$TITLE	$LINK"
done
