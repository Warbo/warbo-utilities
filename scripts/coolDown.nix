{ bash, makeWrapper, procps, psmisc, runCommand, writeScript }:

with {
  raw = writeScript "coolDown" ''
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
};

runCommand "coolDown" { buildInputs = [ makeWrapper ]; } ''
  makeWrapper "${raw}" "$out" --prefix PATH : "${procps}/bin" \
                              --prefix PATH : "${psmisc}/bin"
''
