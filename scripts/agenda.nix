{ bash, coreutils, gcalcli, gnugrep, gnused, wrap }:

wrap {
  name   = "agenda";
  paths  = [ bash coreutils gcalcli gnugrep gnused ];
  vars   = { LC_ALL = "C"; LOCALE = "C"; };
  script = ''
    #!${bash}/bin/bash

    # Start agenda from half an hour ago
       NOW=$(date "+%s")
     DELTA=$((60 * 30))
      THEN=$((NOW - DELTA))
      DATE=$(date -d "@$THEN" "+%Y-%m-%dT%H:%M")
    RESULT=$(gcalcli --military --nocolor agenda "$DATE")
      CODE=$?

    if [[ "x$1" = "xhead" ]]
    then
      echo "$RESULT" | grep '[0-9]' | head -n1 | sed -e 's/  */ /g'
    else
      echo "$RESULT"
    fi

    exit "$CODE"
  '';
}
