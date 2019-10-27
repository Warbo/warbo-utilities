{ bash, feh, wrap, xorg }:

wrap {
  name   = "on";
  paths  = [ bash feh xorg.xrandr ];
  script = ''
    #!${bash}/bin/bash

    # Run when plugging laptop into monitor

    # Set up multiple screens
    if xrandr | grep "VGA1 connected" > /dev/null
    then
      # We're plugged in. Do we have a resolution?
      if ! xrandr | grep "VGA1 connected [0-9][0-9]*" > /dev/null
      then
        # Nope. We need to switch on.
        bash ~/.screenlayout/office.sh
        setBg
      fi
    else
      echo "VGA cable unplugged, not switching on monitor"
    fi

    # Set up keyboard
    sleep 4; date '+%s' > /tmp/keys-last-ask
  '';
}
