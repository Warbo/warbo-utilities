{ bash, wget, wrap, xmlstarlet }:

wrap {
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
}
