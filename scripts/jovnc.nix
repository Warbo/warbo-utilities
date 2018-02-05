{ bash, gnugrep, openssh, sysPing, tightvnc, wrap }:

wrap {
  name   = "jovnc";
  vars   = { inherit sysPing; };
  paths  = [ bash gnugrep openssh tightvnc ];
  script = ''
    #!/usr/bin/env bash

    [[ -n "$JO_HOST" ]] || {
      JO_HOST=debian.local
    }

    ssh -t jo@"$JO_HOST" "pkill x11vnc; DISPLAY=:0 x11vnc" &

    sleep 3

    # shellcheck disable=SC2154
    if OUTPUT=$("$sysPing" -c 1 "$JO_HOST")
    then
      ADDR=$(echo "$OUTPUT" | grep -o "192\.168\.[0-9]*\.[0-9]*" | head -n1)
      echo "ADDRESS: $ADDR"
      vncviewer "$ADDR"
    fi
  '';
}
