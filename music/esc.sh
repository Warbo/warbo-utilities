#!/usr/bin/env bash

# Escape single-quotes on stdin, so stdout is suitable for wrapping in
# single-quotes.
# For example, if given:
#   You've Got Another Thing Coming
# We'll give out:
#   You'\''ve Got Another Thing Coming
# This will act as a single string if wrapped in single-quotes, like:
#   'You'\''ve Got Another Thing Coming'

sed -e "s/'/'\\\\''/g"
