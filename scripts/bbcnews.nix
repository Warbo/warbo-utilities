{ bash, wget, wrap, xmlstarlet }:

wrap {
  name   = "bbcnews";
  paths  = [ bash xmlstarlet wget ];
  script = ''
    #!/usr/bin/env bash
    set -e

    # Fetch BBC News with crap filtered out

    function stripCrap {
      # Remove item elements whose guid url contains the given text
      xmlstarlet ed -d "//guid[contains(text(),'$1')]/.."
    }

    wget -q -O- "http://feeds.bbci.co.uk/news/rss.xml?edition=uk" |
      stripCrap '/sport/'                                         |
      stripCrap '/news/magazine-'                                 |
      stripCrap '/news/entertainment-arts'                        |
      stripCrap '/news/in-pictures'
  '';
}
