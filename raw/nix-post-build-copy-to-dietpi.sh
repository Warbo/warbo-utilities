#!/usr/bin/env bash
set -e
set -f # disable globbing
export IFS=' '
WANT='manjaro'
REMOTE='ssh://pi@dietpi.local'

if [[ "$USER" = "$WANT" ]]
then
    . ~/.bashrc
    if ~/.config/home-manager/commands/pi4.sh > /dev/null
    then
        echo "Uploading paths $OUT_PATHS" 1>&2
        ts -S1
        TMPDIR=/tmp ts nix copy --to "$REMOTE" $DRV_PATH $OUT_PATHS
    else
        echo "DietPi not available, skipping upload" 1>&2
    fi
else
    if [[ "${GIVE_UP:-0}" -eq 1 ]]
    then
        echo "Running as '$USER' instead of '$WANT', aborting" 1>&2
    else
        export GIVE_UP=1  # Avoids infinite recursion
        exec sudo GIVE_UP=1 OUT_PATHS="$OUT_PATHS" DRV_PATH="$DRV_PATH" \
             -u "$WANT" "$0" "$@"
    fi
fi
true
