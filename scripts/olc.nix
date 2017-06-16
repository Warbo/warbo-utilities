{ wrap, writeScript }:

wrap {
  name   = "olc";
  vars   = {
    FF_EXTRA_CODE = writeScript "click.sh" ''
      #!/usr/bin/env bash
      echo "Clicking" 1>&2
      xdotool mousemove 10 10
      sleep 2
      xdotool click 1
      sleep 2
      xdotool click 1
      sleep 10
    '';

    TIMEOUT = "120";
  };
  script = ''
    #!/usr/bin/env bash
    ff "$1"
  '';
}
