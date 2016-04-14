#!/usr/bin/env bash
echo "Removing empty directories in Music/"
find "Music" -type d -exec rmdir --ignore-fail-on-non-empty {} \;
