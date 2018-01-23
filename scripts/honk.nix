{ alsaUtils, bash, wrap }:

wrap {
  name   = "honk";
  paths  = [ alsaUtils bash ];
  vars   = { alert = ../data/alert.wav; };
  script = ''
    #!/usr/bin/env bash
    amixer sset Master unmute > /dev/null

    # shellcheck disable=SC2154
    aplay "$alert" > /dev/null 2>&1
  '';
}
