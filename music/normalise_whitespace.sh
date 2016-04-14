#!/usr/bin/env bash

# Look for dodgy whitespace in filenames, which is either ugly or may lead
# to dupes. Includes 'double  spaces', ' initial spaces' and
# 'spaces before .extensions'
while read -r NAME
do
    [[ -d "$NAME" ]] && {
        echo "'$NAME' has weird whitespace" >> /dev/stderr
        continue
    }
    [[ -f "$NAME" ]] && {
        DIR=$(dirname "$NAME")
        FILE=$(basename "$NAME")
        NORMAL=$(echo "$FILE" | sed -e 's/   */ /g')
        echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
    }
done < <(find Music -name '*  *')
while read -r NAME
do
    [[ -d "$NAME" ]] && {
        echo "'$NAME' has weird whitespace" >> /dev/stderr
        continue
    }
    [[ -f "$NAME" ]] && {
        DIR=$(dirname "$NAME")
        FILE=$(basename "$NAME")
        NORMAL=$(echo "$FILE" | sed -e 's/^  *//g')
        echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
    }
done < <(find Music -name ' *')
while read -r NAME
do
    [[ -d "$NAME" ]] && {
        echo "'$NAME' has weird whitespace" >> /dev/stderr
        continue
    }
    [[ -f "$NAME" ]] && {
        DIR=$(dirname "$NAME")
        FILE=$(basename "$NAME")
        NORMAL=$(echo "$FILE" | sed -e 's/  *\.\([^\.]*\)$/\.\1/g')
        [[ "x$FILE" = "x$NORMAL" ]] ||
            echo "mv -v '${DIR}/${FILE}' '${DIR}/${NORMAL}'"
    }
done < <(find Music -name '* .*')
