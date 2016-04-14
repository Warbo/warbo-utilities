#!/usr/bin/env bash

function free_dirs {
    # Directories which should be in Free rather than Commercial
    for D in "N/Nanowar" "W/Wenlock" "Z/ZX Spectrum" "C/Chiptunes"
    do
        [[ -d "$D" ]] && echo "$D"
    done
    find . -type d -name 'Final Fantasy VII*'
    find . -type d -name 'Newgrounds Audio Portal*'
    find . -type d -name 'Sonic the Hedgehog 2*'
}

function free_out_of_commercial {
    # Move directories which should be in Free out of Commercial
    # (Mostly to appease Jo)
    pushd Music/Commercial > /dev/null
    mkdir -p ../Free
    while read -r DIR
    do
        PARENT=$(dirname "$DIR")
        mkdir -p ../Free/"$PARENT"
        echo "mv -v 'PWD/$DIR' '$PWD/../Free/$DIR'"
    done < <(free_dirs)
    popd > /dev/null
}

free_out_of_commercial
