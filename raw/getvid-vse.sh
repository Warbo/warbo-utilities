#!/usr/bin/env bash
URL=$(lynx -term=linux -accept_all_cookies -cmd_script="$cmd" "$1" |
          grep 'video/mp4' | grep -o 'http[^"]*')
echo "$URL" | grep 'http' && exit 0
exit 1
