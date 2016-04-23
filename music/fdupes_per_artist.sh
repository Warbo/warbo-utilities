#!/usr/bin/env bash

set -e

for LETTER in Music/*
do
    [[ -d "$LETTER" ]] || continue
    for ARTIST in "$LETTER"/*
    do
        [[ -d "$ARTIST" ]] || continue
        fdupes -d -r "$ARTIST"
    done
done
