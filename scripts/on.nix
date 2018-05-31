{ bash, makeWrapper, stdenv, writeScript, xorg }:

with rec {
  raw = writeScript "on" ''
    #!${bash}/bin/bash

    # Run when plugging laptop back in, eg. after a meeting

    # Set up multiple screens
    if xrandr | grep "VGA1 connected" > /dev/null
    then
      # We're plugged in. Do we have a resolution?
      if ! xrandr | grep "VGA1 connected [0-9][0-9]*" > /dev/null
      then
        # Nope. We need to switch on.
        bash ~/.screenlayout/uni.sh
        setBg
      fi
    else
      echo "VGA cable unplugged, not switching on monitor"
    fi

    # Set up keyboard
    sleep 4; keys
  '';
};

stdenv.mkDerivation {
  name         = "on";
  buildInputs  = [ makeWrapper ];
  buildCommand = ''
    #!${bash}/bin/bash
    makeWrapper "${raw}" "$out" --prefix PATH : "${bash}/bin" \
                                --prefix PATH : "${xorg.xrandr}/bin"
  '';
}
