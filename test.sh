#!/usr/bin/env bash
shopt -s nullglob

ERR=0
for TEST in tests/*
do
    if nix-shell -p '(import ./shell.nix)' --show-trace --run "$TEST"
    then
        echo "ok - Passed '$TEST'"
    else
        echo "not ok - Passed '$TEST'"
        ERR=1
    fi
done

exit "$ERR"
