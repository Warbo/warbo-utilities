#!/usr/bin/env bash

echo "Opening Firefox on '$URL'" 1>&2
timeout 30 firefox -safe-mode "$URL" 1>&2 &
FF_PID="$!"
sleep 3

echo "Skipping safe mode prompt" 1>&2
xdotool key --clearmodifiers Return
sleep 10

echo "Opening Web console" 1>&2
xdotool key ctrl+shift+K
sleep 5

echo "Extracting iframe src URLs" 1>&2
xdotool type 'window.prompt("Copy to clipboard: Ctrl+C, Enter", $(".emd_player iframe").toArray().map(function(x){return jQuery(x).attr("src");}).join("\n"));'
sleep 1
xdotool key --clearmodifiers Return
sleep 1

echo "Copying URLs" 1>&2
xdotool key ctrl+c
sleep 1

echo "Pasting URLs" 1>&2
xsel --clipboard
echo ""

kill "$FF_PID"
