#!/usr/bin/env bash
echo "Looking for empty directories in Music/"
find "Music" -type d -empty | while read -r D
do
    ESCAPED=$(echo "$D" | sed -e "@'@'\\''@g")
    echo "rmdir '$ESCAPED'"
done
