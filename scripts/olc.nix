{ phantomjs, wrap, writeScript }:

wrap {
  name   = "olc";
  paths  = [ phantomjs ];
  vars   = {
    EXTRACTOR = writeScript "extractor.js" ''
      // From https://gist.github.com/Tithen-Firion/8b3921d745131837519d5c5b95b86440

      var separator = '\t';
      var page = require('webpage').create(),
        system = require('system'),
        id, match;

      if(system.args.length < 2) {
        console.error('No URL provided');
        phantom.exit(1);
      }
      match = system.args[1].match(
        /https?:\/\/(?:o...l...\.(?:co|io)|ol...\.tv)\/(?:f|embed)\/([\w\-]+)/);
      if(match === null) {
        console.error('Could not find video ID in provided URL');
        phantom.exit(2);
      }
      id = match[1];

      // thanks @Mello-Yello :)
      page.onInitialized = function() {
        page.evaluate(function() {
          delete window._phantom;
          delete window.callPhantom;
        });
      };
      page.settings.userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36";
      page.open(system.args[1], function(status) {
        var info = page.evaluate(function() {
          return {
            decoded_id: document.getElementById('streamurl').innerHTML,
            title: document.querySelector('meta[name="og:title"],'
              + 'meta[name=description]').content
          };
        });
        var url = '/stream/' + info.decoded_id + '?mime=true';
        console.log(url + separator + info.title);
        phantom.exit();
      });
    '';
  };
  script = ''
    #!/usr/bin/env bash
    OUTPUT=$(phantomjs --ssl-protocol=any "$EXTRACTOR" "$@")

    FRAGMENT=""
    NAME="INSERT NAME HERE"
    while read -r RESULT
    do
      BIT=$(echo "$RESULT" | cut -f1)
      if echo "$BIT" | grep '^/stream' > /dev/null
      then
        FRAGMENT="$BIT"
        NAME=$(echo "$RESULT" | cut -f2 | "${../music/esc.sh}")
      fi
    done < <(echo "$OUTPUT")

    [[ -n "$FRAGMENT" ]] || {
      echo "No result, aborting" 1>&2
      echo "Output:" 1>&2
      echo "$OUTPUT" 1>&2
      exit 1
    }
    echo "inDir ~/Public/TODO wget -O '$NAME' '$(cat ~/.olc)$FRAGMENT'"
  '';
}
