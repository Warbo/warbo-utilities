#!/usr/bin/env bash

# Trace program execution:
#  -f                Trace child threads/processes too
#  -e trace=process  Only log process-related events (creation, exit, ...)
#  -ttt              Give absolute UNIX timestamps for each event
#  -q                Don't log tracer attaches/detaches
strace -f -e trace=process -ttt -q "$@"

# NOTE: Trace is logged to stderr, so use 2> to save it
