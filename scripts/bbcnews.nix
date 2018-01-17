{ bash, curl, runCommand, wget, withDeps, wrap, xmlstarlet }:

with builtins;
with rec {
  bbcnews = wrap {
    name   = "bbcnews";
    paths  = [ bash xmlstarlet wget ];
    script = ''
      #!/usr/bin/env bash
      set -e

      echo "Fetching BBC News" 1>&2

      function stripCrap {
        # Remove item elements whose guid url contains the given text
        xmlstarlet ed -d "//guid[contains(text(),'$1')]/.."
      }

      wget -q -O- "http://feeds.bbci.co.uk/news/rss.xml?edition=uk" |
        stripCrap '/sport/'                                         |
        stripCrap '/news/magazine-'                                 |
        stripCrap '/news/entertainment-arts'                        |
        stripCrap '/news/in-pictures'                               |
        stripCrap '/news/av/'
    '';
  };

  tests = attrValues {
    noSport = runCommand "no-sport-test"
    {
      inherit bbcnews;
      buildInputs = [ curl ];
    }
    ''
      set -e

      if curl "http://www.bbc.co.uk" > /dev/null
      then
        echo "Looks like we're online..." 1>&2
      else
        echo "Not online, skipping test" 1>&2
        mkdir "$out"
        exit 0
      fi

      if "$bbcnews" | grep guid | grep sport
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
