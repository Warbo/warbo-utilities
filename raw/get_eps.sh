#!/usr/bin/env bash
set -e

if [[ -e "$2" ]]
then
    PAGE=$(cat "$2")
else
    echo "$2" | grep -q 'epguides.com' ||
        fail 'get_eps URL should be from epguides.com'

    PAGE=$(curl -f "$2") || fail "Couldn't download '$2'"
fi

# shellcheck disable=SC2154
echo "$PAGE" | FEED="$1" "$processor"
