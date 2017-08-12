#! /usr/bin/env nix-shell
#! nix-shell -i bash -p kid3

while read -r FILE
do
    NAME=$(basename "$FILE")
    KNOWN=0
    for SUFFIX in aac mp3 ogg oga opus m4a wma
    do
        if echo "$NAME" | grep -i '\.'"$SUFFIX"'$' > /dev/null
        then
            KNOWN=1
        fi
    done

    if [[ "$KNOWN" -eq 0 ]]
    then
        echo "Don't know how to handle: $FILE"
        continue
    fi

    DIR=$(dirname "$FILE")
    ESC=$(echo "$NAME" | sed -e 's/"/\"/g')

    ARTIST=$(basename "$1")

    echo "Setting artist to '$ARTIST' in '$FILE'" 1>&2

    export DISPLAY=:0
    kid3-cli -c 'select "'"$NAME"'"' -c 'set artist "'"$ARTIST"'"' -c 'save' "$DIR"
done < <(find "$1" -type f)
