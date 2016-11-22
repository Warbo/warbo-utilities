{ bash, findutils, jq, makeWrapper, runCommand, writeScript }:

with {

script = writeScript "failed_tests" ''
  #!${bash}/bin/bash
  shopt -s nullglob

  D="$HOME/System/Tests"

  function count {
      find "$D/$1" -type f -o -type l | wc -l
  }

       ATTRS=$("$D/helpers/tests.sh")
   ATTRCOUNT=$(echo "$ATTRS" | jq 'length')
  ATTRPASSES=$(echo "$ATTRS" | jq 'map(select(.pass)) | length')
   NOTPASSED=$(echo "$ATTRS" | jq -r 'to_entries | map(select(.value.pass | not)) | .[] | .key')

  SCRIPTS=$(count scripts/)
     PASS=$(count results/pass/)
    CHECK=$(count results/check/)
  RUNNING=$(count results/running/)

      ALL=$(( SCRIPTS + ATTRCOUNT ))
  SUCCESS=$(( PASS + CHECK + ATTRPASSES ))

  DIFF=$(( ALL - SUCCESS ))

  HEX="#00FF00"
  [[ "$DIFF" -gt 0 ]] && HEX="#FF0000"

  printf '<fc=%s>%s/%s</fc>\n' "$HEX" "$SUCCESS" "$ALL"

  [[ "x$1" = "xshow" ]] || exit 0

  echo "PASS:    $PASS"
  echo "CHECK:   $CHECK"
  echo "SUCCESS: $SUCCESS"
  echo "RUNNING: $RUNNING"
  echo "ALL:     $ALL"

  echo "Passed:"
  ls "$D/results/pass/"
  echo ""

  echo "To check:"
  ls "$D/results/check/"
  echo ""

  echo "Not run:"
  for TEST in "$D/scripts"/*
  do
      NAME=$(basename "$TEST")
      [[ -e "$D/results/stdout/$NAME" ]] || echo "$NAME"
  done
  while read -r NAME
  do
      [[ -e "$D/results/stdout/$NAME" ]] || echo "$NAME"
  done < <(echo "$NOTPASSED")
  echo ""

  echo "Failures:"
  for TEST in "$D/scripts"/*
  do
      NAME=$(basename "$TEST")
      [[ -e "$D/results/stdout/$NAME" ]] || continue
      [[ -e "$D/results/pass/$NAME"   ]] && continue
      [[ -e "$D/results/check/$NAME"  ]] && continue
      echo "$NAME"
  done
  while read -r NAME
  do
      [[ -e "$D/results/stdout/$NAME" ]] || continue
      [[ -e "$D/results/pass/$NAME"   ]] && continue
      [[ -e "$D/results/check/$NAME"  ]] && continue
      echo "$NAME"
  done < <(echo "$NOTPASSED")
'';

};

runCommand "failed_tests_wrapped" { buildInputs = [ makeWrapper ]; } ''
  #!${bash}/bin/bash
  makeWrapper "${script}" "$out" --prefix PATH : "${jq}/bin" \
                                 --prefix PATH : "${findutils}/bin" \
''
