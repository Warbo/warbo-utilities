#!/usr/bin/env bash

shopt -s nullglob
echo "Looking for YouTube vids" 1>&2
NAMES=()
while read -r NAME
do
    NAMES+=("$NAME")
done < <(grep '^youtube' < "$HOME/.feeds" | cut -f2)

function msg {
    read -r -p "Should we get '$1' video '$2'? (Yes/no/read) " answer
    [[ -z "$answer" ]] && answer=y
    answer=$(echo "$answer" | tr '[:upper:]' '[:lower:]' | cut -c1)
}

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

        msg "$NAME" "$TITLE"
        case "$answer" in
            y)
                echo "Queueing" 1>&2
                pushd "$HOME/Downloads" > /dev/null || exit 1
                    # shellcheck disable=SC2154
                    ts "$youtube_then_mark" "$F" "$URL"
                popd > /dev/null || exit 1
                ;;
            r)
                echo "Marking as read" 1>&2
                markRead "$F"
                ;;
            n)
                echo "Skipping" 1>&2
                ;;
        esac
    done
done

echo "Looking for TED talks" 1>&2
for F in "$HOME"/Mail/feeds/TEDTalks/new/*
do
    TITLE=$(grep '^Subject: ' < "$F" | cut -d ' ' -f2-)
    msg "TED Talk" "$TITLE"
    case "$answer" in
        y)
            URL=$(grep '^Link: ' < "$F" | cut -d ' ' -f2-)
            echo "Queueing" 1>&2
            pushd "$HOME/Downloads" > /dev/null || exit 1
                # shellcheck disable=SC2154
                ts "$youtube_then_mark" "$F" "$URL"
            popd > /dev/null || exit 1
            ;;
        r)
            echo "Marking as read" 1>&2
            markRead "$F"
            ;;
        n)
            echo "Skipping" 1>&2
            ;;
    esac
done

# shellcheck disable=SC2154
"$fetch_podcasts"
