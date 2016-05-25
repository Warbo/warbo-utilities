#!/usr/bin/env bash

echo "$1" | tr '[:upper:]' '[:lower:]' | tr -cd '[:lower:]'
