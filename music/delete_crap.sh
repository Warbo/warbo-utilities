#!/usr/bin/env bash

echo "Looking for crap you might want to delete"
find "Music" -iname "*.db"   \
         -or -iname "*.jpg"  \
         -or -iname "*.jpeg" \
         -or -iname "*.url"  \
         -or -iname "*.txt"  \
         -or -iname "*.ini"  \
         -or -iname "*.onetoc2"
