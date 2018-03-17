{ bash, nettools, procps, psmisc, wrap, xbindkeys, xcape, xorg }:

wrap {
  name   = "keys";
  paths  = [ bash nettools procps psmisc xbindkeys xcape xorg.setxkbmap
             xorg.xmodmap ];
  script = ''
    #!/usr/bin/env bash

    # Enable keyboard shortcuts (eg. volume buttons)
    pgrep xbindkeys > /dev/null || xbindkeys

    # Tries to reduce 'Emacs pinky', caused by the position of Ctrl on PS/2
    # keyboards

    # Put Ctrl back to the correct place on the PS/2 layout. This is where Caps Lock
    # is, so we map that to Ctrl. And nothing of value was lost.
    HOST=$(hostname)

    if [[ "x$HOST" = "xolpc" ]]
    then
      setxkbmap -layout us # OLPC keyboard is US, but Ctrl is correctly placed
    else
      # NixOS does this with configuration.nix, but gets forgotten after Xrandr
      setxkbmap -layout gb -option ctrl:nocaps # GB layout, CapsLock as Ctrl
    fi

    # Kill any existing xcape and xbindkeys instances
    killall xcape     || true
    killall xbindkeys || true

    # Use xmodmap to map space bar to the 'left hyper' key
    spare_modifier="Hyper_L"
    xmodmap -e "keycode 65 = $spare_modifier"

    # Remove the normal 'left hyper' mapping
    # hyper_l is mod4 by default
    xmodmap -e "remove mod4 = $spare_modifier"

    # Map 'left hyper' to Control
    xmodmap -e "add Control = $spare_modifier"

    # Map space to an unused keycode
    xmodmap -e "keycode any = space"

    # Make Alt Gr space
    xmodmap -e "keycode 108 = space"

    # Use xcape to make tapping 'left hyper' produce a space
    xcape -e "$spare_modifier=space"

    # Restart xbindkeys for volume, etc.
    xbindkeys
  '';
}
