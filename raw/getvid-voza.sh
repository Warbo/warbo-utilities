#!/usr/bin/env bash
URL=$(wget -q -O - "$1" | xidel -s -e '//video/source/@src' -)
echo "$URL" | grep 'http' && exit 0
exit 1
