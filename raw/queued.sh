#!/usr/bin/env bash

# Allow a directory to be passed in; we keep going until it's deleted. Basically
# a poor man's message passing.
DIR="$1"
[[ -n "$DIR" ]] || DIR=$(mktemp --tmpdir -d 'check-queue-XXXXX')

# nix-shell seems to use a different task queue than normal
LABEL="$2"
[[ -n "$LABEL" ]] || LABEL="NORMAL"

LAST=0  # Keep track of the latest task we've seen
while [[ -e "$DIR" ]]
do
    while ts 2>&1 | grep running
    do
        # Follow output of running/last-finished task
        ts -t
        sleep 1

        # Get the last task ID
        THIS=$(ts | grep finished | cut -d ' ' -f1 | tail -n1)
        [[ -n "$THIS" ]] || THIS=0

        # Iff this task hasn't been seen before, print out a message. This gives
        # us some indication that things are happening, even if there's no
        # output.
        if [[ "$THIS" = "$LAST" ]]
        then
            true
        else
            echo "FINISHED $LABEL TASK $THIS"
        fi
        LAST="$THIS"

        sleep 3
    done
    sleep 3
done
