{
  alsa-utils,
  bash,
  raw,
  wrap,
}:

wrap {
  name = "honk";
  paths = [
    alsa-utils
    bash
  ];
  vars = {
    alert = raw."alert.wav";
  };
  script = ''
    #!${bash}/bin/bash
    amixer sset Master unmute > /dev/null

    # shellcheck disable=SC2154
    aplay "$alert" > /dev/null 2>&1
  '';
}
