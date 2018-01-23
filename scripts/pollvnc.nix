{ bash, tightvnc, wrap}:

wrap {
  name   = "pollvnc";
  paths  = [ bash tightvnc ];
  script = ''
    #!/usr/bin/env bash
    while true
    do
      if pgrep x11vnc
      then
        vncviewer :0
      else
        sleep 1
      fi
    done
  '';
}
