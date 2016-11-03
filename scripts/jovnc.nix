{ bash, gnugrep, openssh, tightvnc, writeScript }:

writeScript "jovnc" ''
  #!${bash}/bin/bash -p

  [[ -n "$JO_HOST" ]] || {
    JO_HOST=debian.local
  }

  "${openssh}/bin/ssh" -t jo@"$JO_HOST" "pkill x11vnc; DISPLAY=:0 x11vnc" &

  sleep 3

  if OUTPUT=$(/var/setuid-wrappers/ping -c 1 "$JO_HOST")
  then
    ADDR=$(echo "$OUTPUT" | "${gnugrep}/bin/grep" -o "192\.168\.[0-9]*\.[0-9]*" | head -n1)
    echo "ADDRESS: $ADDR"
    "${tightvnc}/bin/vncviewer" "$ADDR"
  fi
''
