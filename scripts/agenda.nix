{ bash, coreutils, gcalcli, gnugrep, gnused, makeWrapper, runCommand }:

runCommand "mk-agenda"
  {
    buildInputs = [ makeWrapper ];
    raw = writeScript "agenda" ''
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
  ''
    #!${bash}/bin/bash
    makeWrapper "$raw" "$out" --prefix PATH : "${coreutils}/bin" \
                              --prefix PATH : "${gcalcli}/bin"   \
                              --prefix PATH : "${gnugrep}/bin"   \
                              --prefix PATH : "${gnused}/bin"
  ''
