{ bash, nettools, procps, writeScript, xbindkeys, xcape, xorg }:

writeScript "keys" ''
  #!${bash}/bin/bash

  # Enable keyboard shortcuts (eg. volume buttons)
  "${procps}/bin/pgrep" xbindkeys > /dev/null || "${xbindkeys}/bin/xbindkeys"

  # Tries to reduce 'Emacs pinky', caused by the position of Ctrl on PS/2
  # keyboards

  # Put Ctrl back to the correct place on the PS/2 layout. This is where Caps Lock
  # is, so we map that to Ctrl. And nothing of value was lost.
  HOST=$(${nettools}/bin/hostname)

  if [[ "x$HOST" = "xolpc" ]]
  then
    setxkbmap -layout us # OLPC keyboard is US, but Ctrl is correctly placed
  else
    # NixOS does this with configuration.nix, but gets forgotten after Xrandr
    setxkbmap -layout gb -option ctrl:nocaps # GB layout, CapsLock as Ctrl
  fi

  # Kill any existing xcape instances
  killall xcape || true

  # Use xmodmap to map space bar to the 'left hyper' key
  spare_modifier="Hyper_L"
  "${xorg.xmodmap}/bin/xmodmap" -e "keycode 65 = $spare_modifier"

  # Remove the normal 'left hyper' mapping
  # hyper_l is mod4 by default
  "${xorg.xmodmap}/bin/xmodmap" -e "remove mod4 = $spare_modifier"

  # Map 'left hyper' to Control
  "${xorg.xmodmap}/bin/xmodmap" -e "add Control = $spare_modifier"

  # Map space to an unused keycode
  "${xorg.xmodmap}/bin/xmodmap" -e "keycode any = space"

  # Make Alt Gr space
  "${xorg.xmodmap}/bin/xmodmap" -e "keycode 108 = space"

  # Use xcape to make tapping 'left hyper' produce a space
  "${xcape}/bin/xcape" -e "$spare_modifier=space"
''
