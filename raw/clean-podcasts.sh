#!/usr/bin/env bash
set -eu

DIR="$HOME/.local/share/gnome-podcasts/Downloads"
[[ -e "$DIR" ]] || {
    echo "No such directory '$DIR'"
    exit 1
} 1>&2

du -sh "$DIR" 1>&2
while read -r F
do
    rm -v "$F"
done < <(find "$DIR" -iname '*.m2a')
du -sh "$DIR" 1>&2
