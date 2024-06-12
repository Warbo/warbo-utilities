{
  bash,
  feh,
  wrap,
  xorg,
}:

wrap {
  name = "off";
  paths = [
    bash
    feh
    xorg.xrandr
  ];
  script = ''
    #!${bash}/bin/bash

    # Run when unplugging laptop, eg. to go to a meeting

    # Turn off external display
    bash ~/.screenlayout/unplugged.sh

    setBg

    # Set up keyboard
    sleep 4; date '+%s' > /tmp/keys-last-ask
  '';
}
