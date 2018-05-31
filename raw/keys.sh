#!/usr/bin/env bash
set -e
set -o pipefail

# Tries to reduce 'Emacs pinky', caused by the position of Ctrl on PS/2
# keyboards. We try to make this command idempotent, so it's safe to run
# over and over.

function msg {
    echo -e "$*" 1>&2
}

# Don't do anything if X isn't running
xdotool get_desktop > /dev/null || {
    msg "No DISPLAY '$DISPLAY', skipping" 1>&2
    exit 0
}

# Put Ctrl back to the correct place on the PS/2 layout. This is where Caps
# Lock is, so we map that to Ctrl. And nothing of value was lost.
# These can be set by configuration.nix in NixOS, but get reset by Xrandr.

# Has caps lock been mapped to ctrl?
NOCAPS=1
setxkbmap -print | grep 'ctrl(nocaps)' > /dev/null || NOCAPS=0

# Are we using GB layout?
GB=1
setxkbmap -print | grep 'pc+gb' > /dev/null || GB=0

# Are we using US layout?
US=1
setxkbmap -print | grep 'pc+us' > /dev/null || US=0

# Which machine are we on?
HOST=$(hostname)

# OLPC has a US keyboard, with ctrl in a sensible place
SET_OLPC=0
if [[ "x$HOST" = "xolpc" ]]
then
    [[ "$US"     -eq 1 ]] || SET_OLPC=1
    [[ "$NOCAPS" -eq 0 ]] || SET_OLPC=1
fi
[[ "$SET_OLPC" -eq 0 ]] || {
    msg "Setting up OLPC keyboard"
    [[ "$NOCAPS" -eq 0 ]] || setxkbmap -option  # Reset options
    [[ "$US"     -eq 1 ]] || setxkbmap -layout us
}
unset SET_OLPC

# We assume everything else is GB with silly ctrl placement
SET_GB=0
[[ "x$HOST" != "xolpc" ]] && [[ "$GB"     -eq 1 ]] || SET_GB=1
[[ "x$HOST" != "xolpc" ]] && [[ "$NOCAPS" -eq 1 ]] || SET_GB=1
[[ "$SET_GB" -eq 0 ]] || {
    msg "Setting up GB keyboard"
    setxkbmap -layout gb -option ctrl:nocaps
}
unset SET_GB
unset NOCAPS
unset GB
unset US

# Use xbindkeys for hotkeys (volume, etc.)
msg "Starting xbindkeys"
killall xbindkeys > /dev/null || true
xbindkeys

# space2ctrl makes holding spacebar act like ctrl (useful for Emacs)
s2cctl stop || true
s2cctl start

# What if we want to hold down a space key? Turn AltGr into another space
xmodmap ~/.Xmodmap
