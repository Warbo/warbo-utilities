{ alsaUtils, bash, coreutils, curl, gcalcli, gnugrep, gnused, jsbeautifier, lib,
  makeWrapper, pidgin, python, phantomjs, runCommand, stdenv, writeScript,
  xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with rec {
  scripts = rec {
    agenda    = runCommand "mk-agenda"
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
      '';
    beeminder = runCommand "mk-beeminder"
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
      '';
    honk = writeScript "honk" ''
      #!${bash}/bin/bash
      ${alsaUtils}/bin/amixer sset Master unmute > /dev/null
      ${alsaUtils}/bin/aplay ${pidgin}/share/sounds/purple/alert.wav > /dev/null 2>&1
    '';
  };

  mkCmd = name: script: ''cp "${script}" "$out/bin/${name}"'';
  cmds  = attrValues (mapAttrs mkCmd scripts);
};

stdenv.mkDerivation {
  name = "warbo-utilities";
  src  = ./.;

  propagatedBuildInputs = [
    python
    phantomjs
    jsbeautifier
    xidel
    xdotool
    xvfb_run
    xsel
  ];

  installPhase = ''
    mkdir -p "$out/bin"
    for DIR in svn system web git development testing docs
    do
      cp "$DIR/"* "$out/bin/"
    done
    ${concatStringsSep "\n" cmds}
  '';
}
