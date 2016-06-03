#!/usr/bin/env bash
shopt -s nullglob

ERR=0
for TEST in tests/*
do
    "$TEST" || ERR=1
done

exit "$ERR"
