#!/usr/bin/env bash

INPUT=$(cat)

if command -v xidel > /dev/null
then
    echo "$INPUT" | xidel -q - -e '//div[@id="album_tabs_tracklist"]//tr[@class="odd" or @class="even"]/(td[position() = 1] || "	" || td[position() = 2])'
elif command -v nix-shell > /dev/null
then
    echo "$INPUT" | nix-shell -p xidel --run "xidel -q - -e '//div[@id=\"album_tabs_tracklist\"]//tr[@class=\"odd\" or @class=\"even\"]/(td[position() = 1] || \"	\" || td[position() = 2])'"
else
    echo "Can't find 'xidel' or 'nix-shell'" 1>&2
fi
