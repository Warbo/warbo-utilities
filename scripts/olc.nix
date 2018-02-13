{ bash, curl, fail, ff, runCommand, wget, withDeps, wrap, writeScript, xdotool,
  xidel }:

with rec {
  scraper = wrap {
    name = "olc-scraper";
    file = ff;
    vars = {
      FF_EXTRA_CODE = wrap {
        name   = "olc-clicky";
        paths  = [ bash xdotool ];
        script = ''
          #!/usr/bin/env bash
          set -e
          sleep 30
          xdotool key ctrl+shift+K
          sleep 5
          xdotool type 'document.getElementById("videooverlay").click();'
          sleep 5
          xdotool key --clearmodifiers Return
          xdotool key ctrl+shift+I
          sleep 5
        '';
      };
    };
  };

  go = wrap {
    name   = "olc";
    paths  = [ bash xidel ];
    vars   = { inherit scraper; };
    script = ''
      #!/usr/bin/env bash

      # shellcheck disable=SC2154
      OUTPUT=$("$scraper" "$@")
      URL=$(echo "$OUTPUT" | xidel -q - -e '//video/@src')
      if [[ -n "$URL" ]]
      then
        echo "https://openload.co$URL"
      else
        echo "No olc URLs found" 1>&2
      fi
    '';
  };

  bunny-test = runCommand "olc-bunny"
    {
      inherit go;
      buildInputs   = [ curl fail wget ];
      SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;
      URL           = "https://openload.co/embed/Iu-HYKyMz5Y/";
    }
    ''
      set -e
      curl "$URL" > /dev/null || {
        echo "WARNING: Couldn't fetch '$URL', maybe offline? Skipping test" 1>&2
        mkdir "$out"
        exit
      }

      FOUND=$("$go" "$URL")
      COUNT=$(echo "$FOUND" | grep '^.' | wc -l)
      [[ "$COUNT" -eq 1 ]] || fail "Expected 1 line, got '$COUNT' from '$FOUND'"

      echo "Checking headers from '$FOUND'" 1>&2
      HEADERS=$(wget --server-response --spider "$FOUND" 2>&1) ||
        fail "Couldn't fetch '$FOUND'"

      TYPE=$(echo "$HEADERS" | grep 'Content-Type')
      echo "$TYPE" | grep 'video' || fail "Expected video, found '$TYPE'"

      mkdir "$out"
    '';
};

/*withDeps [ bunny-test ]*/ go
