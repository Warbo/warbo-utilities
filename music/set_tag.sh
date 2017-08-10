#! /usr/bin/env nix-shell
#! nix-shell -i bash -p kid3

export DISPLAY=:0

TAG="$1"
VAL="$2"

function setTag() {
  NAME=$(basename "$1" | sed -e 's/"/\"/g')
   DIR=$(dirname  "$1")

  kid3-cli -c 'select "'"$NAME"'"' -c 'set "'"$TAG"'" "'"$VAL"'"' -c 'save' "$DIR"
}

shift
shift

for ARG in "$@"
do
    setTag "$ARG"
done
