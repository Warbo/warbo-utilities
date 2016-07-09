#!/usr/bin/env bash
shopt -s nullglob

ERR=0
for TEST in tests/*
do
    if "$TEST"
    then
        echo "ok - Passed '$TEST'"
    else
        echo "not ok - Passed '$TEST'"
        ERR=1
    fi
done

exit "$ERR"
