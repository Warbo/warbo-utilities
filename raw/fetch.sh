#!/usr/bin/env bash

shopt -s nullglob
echo "Looking for YouTube vids" 1>&2
NAMES=()
while read -r NAME
do
    NAMES+=("$NAME")
done < <(grep '^youtube' < "$HOME/.feeds" | cut -f2)

for NAME in "${NAMES[@]}"
do
    DIR="$HOME/Mail/feeds/$NAME"
    [[ -d "$DIR" ]] || continue
    for F in "$DIR"/new/*
    do
        URL=$(grep '^Link: ' < "$F" | head -n1 | grep -o 'http.*' |
              grep 'youtube.com')
        [[ -n "$URL" ]] || continue
        TITLE=$(wget -q -O- "$URL" | xidel -q -e '//h1' - |
                grep -v 'unavailable')
        read -r -p "Should we get '$NAME' video '$TITLE'?" answer
        if [[ -z "$answer" ]] || echo "$answer" | grep -i y > /dev/null
        then
            echo "Queueing" 1>&2
            pushd "$HOME/Downloads" > /dev/null
                # shellcheck disable=SC2154
                ts "$youtube_then_mark" "$F" "$URL"
            popd > /dev/null
        else
            echo "Skipping" 1>&2
        fi
    done
done

echo "Looking for TED talks" 1>&2
for F in "$HOME"/Mail/feeds/TEDTalks/new/*
do
    TITLE=$(grep '^Subject: ' < "$F" | cut -d ' ' -f2-)
    read -r -p "Should we get TED Talk '$TITLE'?" answer
    if [[ -z "$answer" ]] || echo "$answer" | grep -i y > /dev/null
    then
        URL=$(grep '^Link: ' < "$F" | cut -d ' ' -f2-)
        echo "Queueing" 1>&2
        pushd "$HOME/Downloads" > /dev/null
          # shellcheck disable=SC2154
          ts "$youtube_then_mark" "$F" "$URL"
        popd > /dev/null
    else
        echo "Skipping" 1>&2
    fi
done

# shellcheck disable=SC2154
"$fetch_podcasts"
