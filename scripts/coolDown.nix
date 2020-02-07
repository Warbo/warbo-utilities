{ bash, procps, psmisc, wrap }:

wrap {
  name   = "coolDown";
  paths  = [ bash procps psmisc ];
  script = ''
    #!${bash}/bin/bash

    function setState {
      while read -r P
      do
        if pgrep "$P" > /dev/null
        then
          killall -s "$1" "$P"
        fi
      done < /home/chris/.coolDown
    }

    while true
    do
      if hot
      then
        setState STOP
      else
        setState CONT
      fi

      sleep 20
    done
  '';
}
