#!/usr/bin/env bash

if bbcnews | grep guid | grep sport
then
    echo "Didn't filter out sport" 1>&2
    exit 1
fi
exit 0
