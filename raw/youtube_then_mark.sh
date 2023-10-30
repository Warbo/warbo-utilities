#!/usr/bin/env bash
set -e

F="$1"
[[ -e "$F" ]] || {
    echo "Given file '$1' not found, aborting" 1>&2
    exit 1
}

function get {
    COUNTDOWN=10
    # shellcheck disable=SC2154
    while ! timeout 120 "$yt" "$@"
    do
        sleep 1
        COUNTDOWN=$(( COUNTDOWN - 1 ))
        [[ "$COUNTDOWN" -lt 1 ]] && break
    done
    if [[ "$COUNTDOWN" -lt 1 ]]
    then
        return 1
    else
        return 0
    fi
}

shift
get "$@" && markRead "$F"
