{ bash, html2text, makeWrapper, runCommand, wget, wrap, xidel, xmlstarlet }:

wrap {
  name   = "get-eps";
  paths  = [ bash html2text wget xidel xmlstarlet ];
  script = ''
    #!/usr/bin/env bash
     PAGE=$(wget -q -O- "$2")
    DATES=$(echo "$PAGE" | xidel -q - -e '//table//tr/td[5]' |
                           grep ' ('                         |
                           grep -o '([0-9-]*)'               |
                           grep -o '[0-9-]*')
    TITLE=$(echo "$PAGE" | xidel -q - -e '//title/text()')
    FEED=$(mktemp '/tmp/get-eps-XXXXX.xml')
    cat <<EOF > "$FEED"
    <?xml version="1.0" encoding="utf-8"?>
    <rss xmlns:atom="http://www.w3.org/2005/Atom" version="2.0">
      <channel>
        <title>$1</title>
        <description>$TITLE</description>
      </channel>
    </rss>
    EOF

    NOW=$(date -d yesterday +%s)
    while read -r DATE
    do
      SECS=$(date -d"$DATE" +%s)
      if [[ "$SECS" -lt "$NOW" ]]
      then
        # Look up the row with this date, and get the episode number
        NUM=$(echo "$PAGE" | tr -d '\n'                 |
                             sed -e 's@<tr@\n<tr@g'     |
                             sed -e 's@</tr>@</tr>\n@g' |
                             grep '^<tr'                |
                             grep "$DATE"               |
                             xidel -q - -e '//th/text()')
        xmlstarlet ed -L \
          -a "//channel" -t elem -n item        -v ""             \
          -s "//item[1]" -t elem -n title       -v "Episode $NUM" \
          -s "//item[1]" -t elem -n link        -v "$2"           \
          -s "//item[1]" -t elem -n pubDate     -v "$DATE"        \
          -s "//item[1]" -t elem -n description -v "Episode $NUM" \
          -s "//item[1]" -t elem -n guid        -v "$TITLE-$NUM"  "$FEED"
      fi
    done < <(echo "$DATES")

    cat "$FEED"
    rm "$FEED"
  '';
}
