{ bash, chromium-repl, fail, jq, wrap }:

wrap {
  name   = "sleep-dump-page";
  paths  = [ bash fail jq ];
  vars   = { cr = chromium-repl; };
  script = ''
    #!/usr/bin/env bash
    set -e

    [[ -n "$TIMEOUT" ]] || TIMEOUT=30

    function genInput {
      printf "Waiting for Chromium to load page" 1>&2
      for N in $(seq 1 "$TIMEOUT")
      do
        printf "." 1>&2
        echo ""
        sleep 1
      done
      [[ -z "$EXTRA" ]] || {
        echo "Generating extra input $EXTRA" 1>&2
        "$EXTRA"
      }
      echo "Extracting HTML" 1>&2
      echo 'document.body.outerHTML'
      echo "Closing Chromium" 1>&2
      echo 'quit'
    }

    OUTPUT=$(genInput | "$cr" "$@")
     FOUND=$(echo "$OUTPUT" |
             jq -s 'map(select(.result | .type | . == "string"))')

    echo "Checking we found the HTML source" 1>&2
    echo "$FOUND" | jq -e 'length | . > 0' > /dev/null || {
      echo "$OUTPUT" 1>&2
      fail "No HTML found"
    }

    echo "Dumping out page source" 1>&2
    echo "$FOUND" | jq -r '.[] | .result | .value'
  '';
}
