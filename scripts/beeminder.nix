{ bash, coreutils, curl, gnugrep, gnused, makeWrapper, runCommand, writeScript,
  xidel }:

runCommand "mk-beeminder"
  {
    buildInputs = [ makeWrapper ];
    raw = writeScript "beeminder" ''
      #!${bash}/bin/bash

      F=/tmp/bm
      GOT=0
      if [[ -e "$F" ]]
      then
        MOD=$(stat -L --format %Y "$F")
        NOW=$(date +%s)
        AGE=$(( NOW - MOD ))
        if [[ "$AGE" -lt 3600 ]]
        then
          GOT=1
        fi
      fi

      [[ "$GOT" -eq 1 ]] || {
        unset SSL_CERT_FILE
        curl "https://www.beeminder.com/warbo" > "$F"
      }

      GOALS=$(xidel - -q -e \
              '//div[@class="info"]/(.//a | .//span[@class="countdown"])' \
              < "$F")

      NAMES=$(echo "$GOALS" | grep -v " due ")
      TIMES=$(echo "$GOALS" | grep    " due "       |
                              sed -e 's/.* due //g' |
                              sed -e 's/ by .*//g'  )

      OUT=""
      while read -r NAME TIME
      do
           D=$(date -d "$TIME" "+%s")
        LEFT=$(( (D - NOW) / (60 * 60 * 24) ))
        [[ -z "$OUT" ]] || OUT="$OUT, "
        OUT="$OUT$NAME $LEFT"
      done < <(paste <(echo "$NAMES") <(echo "$TIMES"))

      echo "$OUT"
    '';
  }
  ''
    #!${bash}/bin/bash
    makeWrapper "$raw" "$out" --suffix PATH : "${coreutils}/bin" \
                              --suffix PATH : "${curl}/bin"      \
                              --suffix PATH : "${gnugrep}/bin"   \
                              --suffix PATH : "${gnused}/bin"    \
                              --suffix PATH : "${xidel}/bin"
  ''
