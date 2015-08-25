#!/usr/bin/env bash
ERR=0
for TEST in tests/*
do
    "$TEST" || ERR=1
done

exit "$ERR"
