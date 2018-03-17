#!/usr/bin/env bash

if [[ "$#" -lt 1 ]]
then
    echo "Reading links from stdin" 1>&2
    LINKS=$(cat)
else
    echo "Fetching links from '$1'" 1>&2
    LINKS=$(curl -s "$1" |
      xidel -q -e '//div[@class="site"]/a[@data-hostname="vidzi.tv"]/@href' -)
fi

while read -r LINK
do
    while read -r VID
    do
        echo "youtube-dl '$VID'"
    done < <(curl -s "$LINK" | xidel -q -e '//a/@href' - | grep 'vidzi.tv')
done < <(echo "$LINKS")
