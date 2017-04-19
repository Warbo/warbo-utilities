{ makeWrapper, runCommand, wget, writeScript, xmlstarlet }:

# Fetch BBC News with crap filtered out
runCommand "bbcnews"
  {
    buildInputs = [ makeWrapper ];
    raw         = writeScript "bbcnews-raw" ''
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
  ''
    makeWrapper "$raw" "$out" --prefix PATH : "${xmlstarlet}/bin" \
                              --prefix PATH : "${wget}/bin" \
  ''
