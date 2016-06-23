#!/usr/bin/env bash

BASE=$(dirname "$(readlink -f "$0")")

# For each directory at the artist level, we 'simplify' the name to see if
# it collides with another, eg. "AC-DC" and "ACDC" both become "acdc"
echo "Looking for possible dupes in Music/Commercial" 1>&2

ALLOWED=".allowed_artist_dupes"
[[ -e "$ALLOWED" ]] || {
    echo "Making empty '$ALLOWED'" 1>&2
    touch "$ALLOWED"
}

for INIT in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
do
    echo "$INIT" 1>&2
    find "Music/Commercial/$INIT/" -maxdepth 1 |
       "$BASE/list_dupe_guesses.sh"            |  # "n1\tn2" for potential dupes
       grep -F -x -v -f "$ALLOWED"                # Ignore known non-dupes
done
