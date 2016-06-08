#!/usr/bin/env bash

INPUT=$(cat)

if command -v xidel > /dev/null
then
    echo "$INPUT" | xidel -q - -e '//td/a[@class="album"]/(text() || "	" || @href)'
elif command -v nix-shell > /dev/null
then
    echo "$INPUT" | nix-shell -p xidel --run "xidel -q - -e '//td/a[@class=\"album\"]/(text() || \"	\" || @href)'"
else
    echo "'xidel' not available, and neither is 'nix-shell'" 1>&2
fi
