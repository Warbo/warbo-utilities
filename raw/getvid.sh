#!/usr/bin/env bash
set -e

if [[ "$1" = "--help" ]]
then
  # shellcheck disable=SC2154
  echo "$msg" 1>&2
  exit 0
fi

echo "Run with --help as the only arg to see usage and handler scripts" 1>&2

function esc {
  # shellcheck disable=SC1003
  sed -e "s/'/'"'\\'"'''/g"
}

# shellcheck disable=SC2154
LINKS=$("$list" "$@")

echo "LINKS: $LINKS" 1>&2

function tryScrape {
     LINK="$1"
      TIT="$2"
    REGEX="$3"
  SCRAPER="$4"

  echo "$LINK" | grep -q "$REGEX" || return 1

  [[ -n "$DEBUG" ]] && echo "Running $SCRAPER on $LINK" 1>&2
  URL=$("$SCRAPER" "$LINK") || return 0

  [[ -n "$URL" ]] || return 0
  URL=$(echo "$URL" | esc)

  echo "wget --no-check-certificate -c -O '$TIT' '$URL'"
  return 0
}

echo "$LINKS" | while read -r PAIR
do
  LINK=$(echo "$PAIR" | cut -f2)
  [[ -n "$LINK" ]] || continue

  TITLE=$(echo "$PAIR" | cut -f1 | esc)
  [[ -n "$TITLE" ]] || TITLE="UNKNOWN"

  [[ -n "$DEBUG" ]] && echo "Checking $LINK" 1>&2

  # Try "simulating" a youtube-dl run; if it works, present it as an option
  if youtube-dl --no-check-certificate -s "$LINK" > /dev/null 2>&1
  then
    URL=$(echo "$LINK" | esc)
    echo "youtube-dl --no-check-certificate --output '$TITLE' '$URL'"
  fi
  URL=""

  # shellcheck disable=SC2154
  tryScrape "$LINK" "$TITLE" 'x5[4-6][4-6]\.c' "$f5"   && continue

  # shellcheck disable=SC2154
  tryScrape "$LINK" "$TITLE" '/vi..z.\.net/'   "$voza" && continue

  # shellcheck disable=SC2154
  tryScrape "$LINK" "$TITLE" '/vs...e\.e'      "$vse"  && continue
done
