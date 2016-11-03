{ alsaUtils, bash, pidgin, writeScript }:

writeScript "honk" ''
  #!${bash}/bin/bash
  ${alsaUtils}/bin/amixer sset Master unmute > /dev/null
  ${alsaUtils}/bin/aplay ${pidgin}/share/sounds/purple/alert.wav > /dev/null 2>&1
''
