#!/usr/bin/env bash
BASE=$(dirname "$(readlink -f "$0")")

function hasSuffix() {
    if echo "$2" | grep -i "\.$1"'$' > /dev/null
    then
        return 0
    else
        return 1
    fi
}

while read -r FILE
do
    if hasSuffix "mp3"  "$FILE" ||
       hasSuffix "ogg"  "$FILE" ||
       hasSuffix "oga"  "$FILE" ||
       hasSuffix "opus" "$FILE" ||
       hasSuffix "wma"  "$FILE" ||
       hasSuffix "m4a"  "$FILE"
    then
        TAGS=$("$BASE/tags_of.sh" "$FILE")
        if echo "$TAGS" | grep -i 'title' > /dev/null
        then
            continue
        fi
        printf 'Untagged\t'\''%s'\''\n' "$(echo "$FILE" | "$BASE/esc.sh")"
        echo "$TAGS" 1>2
    else
        echo "Don't know format of '$FILE'" 1>&2
    fi
done < <(find Music/Commercial -type f)
