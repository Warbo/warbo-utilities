#! /usr/bin/env nix-shell
#! nix-shell -i bash -p libav -p ffmpeg
set -e

function esc {
    sed -e "s/'/'\\\\''/g"
}

[[ -n "$1" ]] ||  {
    echo "Usage: tag_album_dir.sh path/to/ARTIST/ALBUM" 1>&2
    exit 1
}

[[ -d "$1" ]] || {
    echo "No such directory '$1'" 1>&2
    exit 1
}

ALBUM=$(basename "$1")
ARTIST=$(basename "$(dirname "$1")")

echo "Tagging everything in '$1' as '$ALBUM' by '$ARTIST'" 1>&2
for COUNT in 3 2 1
do
    echo "$COUNT..." 1>&2
done

find "$1" -type f | while read -r SRC
do
    DIR=$(dirname "$(readlink -f "$SRC")")
    NAME=$(basename "$SRC")
    DST="$DIR/tmp_$NAME"

    SRC_ESC=$(echo "$SRC" | esc)
    DST_ESC=$(echo "$DST" | esc)

    ffmpeg -i "$SRC" -acodec copy     \
           -metadata artist="$ARTIST" \
           -metadata album="$ALBUM"   \
           "$DST" > /dev/null 2> /dev/null < /dev/null

    SRC_CRC=$(avconv -i "$SRC" -f crc - 2> /dev/null)
    DST_CRC=$(avconv -i "$DST" -f crc - 2> /dev/null)

    if [[ "x$SRC_CRC" = "x$DST_CRC" ]]
    then
        echo "cp -a '$DST_ESC' '$SRC_ESC'; rm '$DST_ESC'"
    else
        echo "Error: Tagged file '$DST_ESC' has CRC '$DST_CRC', while '$SRC_ESC' has '$SRC_CRC'" 1>&2
    fi
done
