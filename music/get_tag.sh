#! /usr/bin/env nix-shell
#! nix-shell -i bash -p kid3

export DISPLAY=:0

TAG="$1"

function getTag() {
    NAME=$(basename "$1" | sed -e 's/"/\"/g')
     DIR=$(dirname  "$1")

    kid3-cli -c 'select "'"$NAME"'"' -c 'get "'"$TAG"'"' "$DIR"
}

shift

for ARG in "$@"
do
    getTag "$ARG"
done
