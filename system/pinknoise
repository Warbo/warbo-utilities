#! /usr/bin/env nix-shell
#! nix-shell -i bash -p sox

# Various noise generators, mainly from http://unreasonable.org/node/303

# A more calming pink noise; filtered to reduce pops, tremolo to make it wavy
#play -n synth '7:00:00' pinknoise band -n 1200 200 tremolo 20 .1

# Pink noise, filtered to overlap voices
play -c 2 -n synth pinknoise 1> /dev/null 2> /dev/null
