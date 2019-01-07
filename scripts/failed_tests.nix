{ bash, findutils, jq, wrap }:

wrap {
  name   = "failed_tests";
  paths  = [ bash findutils jq ];
  script = ''
    #!/usr/bin/env bash
    shopt -s nullglob

    D="$HOME/System/Tests"

    PTHS=$(cat "$D/results/attrs.json")

        ALL=0
    SUCCESS=0
       DIFF=0
    while read -r PTH
    do
      ALL=$(( ALL + 1 ))
      if [[ -f "$D/results/pass/$PTH" ]]
      then
        SUCCESS=$(( SUCCESS + 1 ))
      else
        DIFF=$(( DIFF + 1 ))
      fi
    done < <(echo "$PTHS")

    HEX="#00FF00"
    [[ "$DIFF" -gt 0 ]] && HEX="#FF0000"

    printf '<fc=%s>%s/%s</fc>\n' "$HEX" "$SUCCESS" "$ALL"

    [[ "x$1" = "xshow" ]] || exit 0

    echo
    echo "Passed:"
    while read -r PTH
    do
      if [[ -f "$D/results/pass/$PTH" ]]
      then
        echo "$PTH"
      fi
    done < <(echo "$PTHS")

    echo
    echo "Not run:"
    while read -r PTH
    do
      if [[ -f "$D/results/pass/$PTH" ]] ||
         [[ -f "$D/results/fail/$PTH" ]]
      then
        continue
      else
        echo "$PTH"
      fi
    done < <(echo "$PTHS")

    echo
    echo "Failures:"
    while read -r PTH
    do
      if [[ -f "$D/results/fail/$PTH" ]]
      then
        echo "$PTH"
      fi
    done < <(echo "$PTHS")
  '';
}
