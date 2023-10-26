#!/usr/bin/env bash
set -e
set -o pipefail

# Reduce 'Emacs pinky', caused by the position of Ctrl on PS/2 keyboards

# Don't do anything if X isn't running
xdotool get_desktop > /dev/null || {
    echo "No DISPLAY '$DISPLAY', skipping" 1>&2
    exit 0
}

# Stop keybindings and remappers if running
s2cctl  stop                  || true
killall s2c       > /dev/null || true
killall xbindkeys > /dev/null || true

# Put Ctrl back to the correct place on the PS/2 layout. This is where Caps
# Lock is, so we map that to Ctrl. And nothing of value was lost.
# These can be set by configuration.nix in NixOS, but get reset by Xrandr.

# Which machine are we on?
HOST=$(hostname)

if [[ "$HOST" = "olpc" ]]
then
    # OLPC has a US keyboard, with ctrl in a sensible place
    setxkbmap -option  # Reset options
    setxkbmap -layout us
else
    # We assume everything else is GB with silly ctrl placement
    setxkbmap -layout gb -option ctrl:nocaps
fi

xbindkeys           # Hotkeys for volume, etc.
s2cctl start        # space2ctrl: makes holding spacebar act like ctrl
xmodmap ~/.Xmodmap  # Custom remappings
