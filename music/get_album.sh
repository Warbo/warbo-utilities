#!/usr/bin/env bash
set -e

BASE=$(dirname "$(readlink -f "$0")")

[[ -d Music/Commercial ]] || {
    echo "Can't find Music/Commercial; check working directory" 1>&2
    exit 1
}

for A in "$1" "$2" "$3"
do
    [[ -n "$A" ]] || {
        echo "Usage: get_album.sh ARTIST ALBUM URL" 1>&2
        exit 1
    }
done

ARTIST="$1"
ALBUM="$2"
URL="$3"

echo "Fetching '$ALBUM' by '$ARTIST' from URL '$URL'. Kill me if wrong..." 1>&2

for COUNT in 3 2 1
do
    echo "$COUNT..."
    sleep 5
done

INIT=$(echo "$1" | cut -c 1 | tr '[:lower:]' '[:upper:]')
DIR="Music/Commercial/$INIT/$ARTIST/$ALBUM"

mkdir -p "$DIR"
pushd "$DIR" > /dev/null

if command -v "ts" > /dev/null 2>/dev/null
then
    # Note: the best audio format may be a video format; we can sort these after
    ts youtube-dl -i -f bestaudio "$URL"
    echo "Download is queued" 1>&2
    ts "$BASE/tag_album_dir.sh" "$(readlink -f .)"
else
    echo "ts not found, fetching directly..." 1>&2
    youtube-dl -i -x "$URL"
    "$BASE/tag_album_dir.sh" "$(readlink -f .)"
fi
