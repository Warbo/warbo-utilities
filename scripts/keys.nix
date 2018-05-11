{ bash, nettools, procps, psmisc, wrap, xbindkeys, xcape, xdotool, xorg }:

wrap {
  name   = "keys";
  paths  = [ bash nettools procps psmisc xbindkeys xcape xdotool xorg.setxkbmap
             xorg.xmodmap ];
  script = ''
    #!/usr/bin/env bash
    set -e
    set -o pipefail

    # Tries to reduce 'Emacs pinky', caused by the position of Ctrl on PS/2
    # keyboards. We try to make this command idempotent, so it's safe to run
    # over and over.

    # Don't do anything if X isn't running
    xdotool get_desktop > /dev/null || {
      echo "No DISPLAY '$DISPLAY', skipping" 1>&2
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
    if [[ "x$HOST" = "xolpc" ]]
    then
      [[ "$US"     -eq 1 ]] || setxkbmap -layout us
      [[ "$NOCAPS" -eq 0 ]] || setxkbmap -option  # Reset options
    else
      # We assume everything else is GB with silly ctrl placement
      [[ "$GB"     -eq 1 ]] || setxkbmap -layout gb -option ctrl:nocaps
      [[ "$NOCAPS" -eq 1 ]] || setxkbmap -layout gb -option ctrl:nocaps
    fi

    # We use xmodmap to override a few keys. Note that triggering setxkbmap
    # above will override these.

    function key {
      xmodmap -pke | grep "keycode $1" | grep "$2" > /dev/null || return 1
    }

    function mod {
      xmodmap -pm | grep "$1" | grep "$2" > /dev/null || return 1
    }

    # We don't have a 'left hyper' key, so we can use that as a "spare"
    spare="Hyper_L"

    # Turn Use xmodmap to map space bar to the spare modifier
    key 65 "$spare" || xmodmap -e "keycode 65 = $spare"

    # Remove the normal spare mapping. Hyper_L is mod4 by default
    mod mod4 "$spare" && xmodmap -e "remove mod4 = $spare"

    # Map spare modifier to Control
    mod 'control' "$spare" || xmodmap -e "add Control = $spare"

    # Map space to an unused keycode
    key any space || xmodmap -e "keycode any = space"

    # Make Alt Gr space
    key 108 space || xmodmap -e "keycode 108 = space"

    # Use xcape to make tapping 'left hyper' produce a space
    pgrep -f xcape > /dev/null || xcape -e "$spare=space"

    # Use xbindkeys for volume, etc.
    killall xbindkeys > /dev/null || true
    xbindkeys
  '';
}
