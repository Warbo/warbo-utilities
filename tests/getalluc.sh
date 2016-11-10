#!/usr/bin/env bash

MSG="Found video URL"

if STOPONFIRST=1 getalluc big buck bunny host:vidzi.tv | grep -m 1 "wget"
then
    echo "ok - $MSG"
    exit 0
fi

echo "not ok - $MSG"
exit 1
