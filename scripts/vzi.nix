{ bash, curl, fail, jsbeautifier, runCommand, wget, withDeps, wrap, xidel }:

with rec {
  untested = wrap {
    name   = "vzi";
    paths  = [ bash curl fail jsbeautifier xidel ];
    vars   = { SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt; };
    script = ''
      #!/usr/bin/env bash
      set -e

      [[ "$#" -eq 1 ]] || fail "vzi needs a URL argument"

      PAGE=$(curl "$1") || fail "vzi couldn't fetch '$1'"

      while read -r LINE
      do
        echo "$LINE" | grep 'p,a,c,k,e,d' > /dev/null || continue

        CONTENT=$(echo "$LINE" | js-beautify -i) ||
          fail "Couldn't beautify\n$LINE"

        echo "$CONTENT" | grep -o 'http[^"]*.mp4'
      done < <(echo "$PAGE" | xidel -q - -e '//script[contains(., "eval")]')
    '';
  };

  test = runCommand "vzi-test"
    (rec {
      inherit untested;
      buildInputs   = [ curl fail wget ];
      SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;
      DOMAIN        = "https://vid" + "zi" + ".tv";
      URL           = DOMAIN + "/77t3xnzgnq9x.html";
    })
    ''
      curl "$DOMAIN" > /dev/null || {
        echo "WARNING: Couldn't fetch '$DOMAIN' (offline?); skipping test" 1>&2
        mkdir "$out"
        exit 0
      }

      curl "$URL" > /dev/null || fail "Failed to load '$URL'"

      OUTPUT=$("$untested" "$URL") || fail "Getting vids from '$URL' failed"

      COUNT=$(echo "$OUTPUT" | grep -c '^http')
      [[ "$COUNT" -eq 1 ]] || fail "Found '$COUNT' videos on '$URL'"

      HEADERS=$(wget --server-response --spider "$OUTPUT" 2>&1) ||
        fail "Couldn't fetch headers for video '$OUTPUT' from '$URL'"

      TYPE=$(echo "$HEADERS" | grep 'Content-Type')

      ACCEPTABLE=0
      for ALLOWED in video octet-stream
      do
        echo "$TYPE" | grep "$ALLOWED" > /dev/null && ACCEPTABLE=1
      done

      [[ "$ACCEPTABLE" -eq 1 ]] || fail "Expected video, found '$TYPE'"

      mkdir "$out"
    '';
};

withDeps [ test ] untested
