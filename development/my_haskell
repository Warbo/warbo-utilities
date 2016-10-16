#!/usr/bin/env bash

# List the Haskell projects in ~/Programming/Haskell which I can edit

skipDirs=(tip-tools IsaHipster
          hipspec LazySmallCheck2012 hbmc quickspec
          hip ghc structural-induction ebc
          unification LC test-data)

function filterDir {
    for PART in $(echo "$1" | sed 's@/@\n@g')
    do
        for SKIP in "${skipDirs[@]}"
        do
            if [[ "x$PART" = "x$SKIP" ]]
            then
                return
            fi
        done
    done
    echo "$1"
}

function allDirs {
    for DIR in ~/Programming/Haskell/*
    do
        [[ -d "$DIR" ]] && echo "$DIR"
    done
    locate -e "/home/chris/Programming/Haskell/*.cabal" | while read -r LINE
    do
        dirname "$LINE"
    done
}

while IFS= read -r POSSIBLE
do
    filterDir "$POSSIBLE"
done < <(allDirs | sort -u)
