{ bash, curl, fail, html2text, python, runCommand, wget, withDeps, wrap, xidel,
  xmlstarlet }:

with builtins;
with rec {
  getContent = wrap {
    name   = "getBBCContent.py";
    file   = ../raw/getBBCContent.py;
    vars   = { SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt; };
    paths  = [
      html2text
      (python.withPackages (p: [ p.beautifulsoup4 p.feedparser p.PyRSS2Gen ]))
    ];
  };

  bbcnews = wrap {
    name   = "bbcnews";
    paths  = [ bash xmlstarlet wget ];
    vars   = { inherit getContent;  };
    script = ''
      #!/usr/bin/env bash
      set -e

      echo "Fetching BBC News" 1>&2

      function stripCrap {
        # Remove item elements whose guid url contains the given text
        xmlstarlet ed -d "//guid[contains(text(),'$1')]/.."
      }

      # shellcheck disable=SC2154
      wget -q -O- "http://feeds.bbci.co.uk/news/rss.xml?edition=uk" |
        stripCrap '/sport/'                                         |
        stripCrap '/news/magazine-'                                 |
        stripCrap '/news/entertainment-arts'                        |
        stripCrap '/news/in-pictures'                               |
        stripCrap '/news/av/'                                       |
        "$getContent"
    '';
  };

  tests = attrValues {
    getContent = runCommand "test-get-content"
      {
        inherit getContent;
        buildInputs  = [ fail xidel ];
        HTML_EXAMPLE = ../raw/bbcExamplePage.html.gz;
        RUN_TESTS    = "1";
      }
      ''
        "$getContent"
        mkdir "$out"
      '';

    noSport = runCommand "no-sport-test"
      {
        inherit bbcnews;
        buildInputs = [ curl ];
      }
      ''
        set -e

        if curl -s "http://www.bbc.co.uk" > /dev/null
        then
          echo "Looks like we're online..." 1>&2
        else
          echo "Not online, skipping test" 1>&2
          mkdir "$out"
          exit 0
        fi

        if "$bbcnews" | grep guid | grep '/sport/'
        then
          echo "Didn't filter out sport" 1>&2
          exit 1
        fi

        echo "Sport was filtered out correctly" 1>&2
        mkdir "$out"
      '';
  };
};
withDeps tests bbcnews
