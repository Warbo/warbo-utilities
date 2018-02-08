{ bash, curl, ff, jsbeautifier, lib, olc, procps, wget, wrap, xidel }:

with builtins;
with lib;
with rec {
  scrapepage = wrap {
    name   = "scrapepage";
    paths  = [ bash curl jsbeautifier xidel ];
    vars   = { inherit ff; };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Try a basic curl request, to get the raw source
      CONTENT1=$(curl -s "$1") || CONTENT1=""

      # Load with Firefox, in case Javascript modifies the page after load
      echo "Scraping '$1' with Firefox" 1>&2
      # shellcheck disable=SC2154
      CONTENT2=$("$ff" "$1") || CONTENT2=""

      # Try to de-obfuscate Javascript with js-beautify, to find more URLs
      CONTENT3=""
      EVALTEMP=$(echo -e "$CONTENT1"                           |
                 xidel -q - -e '//script[contains(., "eval")]' |
                 js-beautify -i)
      CONTENT3=$(echo -e "$CONTENT3\n$EVALTEMP")

      EVALTEMP=$(echo -e "$CONTENT2"                           |
                 xidel -q - -e '//script[contains(., "eval")]' |
                 js-beautify -i)
      CONTENT3=$(echo -e "$CONTENT3\n$EVALTEMP")

      # Take everything we've got and look for video URLs
      CONTENT=$(echo -e "$CONTENT1\n$CONTENT2\n$CONTENT3")

      VIDSRC1=$(echo "$CONTENT1" | xidel -q - -e '//video/@src') || true
      VIDSRC2=$(echo "$CONTENT2" | xidel -q - -e '//video/@src') || true

      VIDURLS=""
      for VID in mp4 flv avi
      do
        VIDS=$(echo "$CONTENT" | grep -i "\.$VID") || true
        NARROWED=""
        while read -r CANDIDATE
        do
          # If we find '"foo.$VID"' then use that, otherwise use the whole line
          NARROW=$(echo "$CANDIDATE" | grep -vo "\"[^\"]*\.$VID[^\"]*\"") ||
            true
          [[ -n "$NARROW" ]] || NARROW="$CANDIDATE"

          NARROWED=$(echo -e "$NARROWED\n$NARROW")
        done < <(echo "$VIDS")

        VIDURLS=$(echo -e "$VIDURLS\n$NARROWED")
      done

      set +e
      # Try to narrow-down output lines to just URLs
      OUTURLS=$(echo -e "$VIDSRC1\n$VIDSRC2\n$VIDURLS" |
                grep -o 'http[^ "<>]*'                 |
                grep -o '[^"]*'                        |
                grep -v 'ads.ad-center.com')
      set -e

      echo "$OUTURLS" | sort -u
    '';
  };
};

wrap {
  name   = "vidsfrompage";
  paths  = [ bash procps wget xidel ];
  vars   = { inherit ff olc scrapepage; };
  script = ''
    #!/usr/bin/env bash
    set -e

    # Look for raw URLs
    CONTENT1=$(wget -q -O- "$1") || CONTENT1=""
    LINKS1=$(echo "$CONTENT1" | grep -o 'http[^ "]*mp4') || LINKS1=""
    echo "$LINKS1"

    # Runs at most one iteration; allows us to break early
    LINKS2=""
    while true
    do
      echo "Looking for videos on '$1' with Firefox" 1>&2
      # shellcheck disable=SC2154
      CONTENT2=$("$ff" "$1") || break

      if [[ -n "$DEBUG" ]]
      then
        echo -e "Source code dump:\n$CONTENT2\nEnd source code" 1>&2
      fi
      IFRAMES=$(echo "$CONTENT2" | xidel - -q -e '//iframe/@src') || break

      LINKS2=$(echo "$IFRAMES" | grep '^http') || break

      echo -e "Found iframes for:\n$LINKS2" 1>&2
      break
    done

    # Loop over result links, getting videos and obfuscated javascript
    while read -r LNK
    do
      [[ -n "$LNK" ]] || continue

      SKIP=0
      for PAT in recaptcha "luc.ee#"
      do
        if echo "$LNK" | grep "$PAT" > /dev/null
        then
          SKIP=1
        fi
      done
      if [[ "$SKIP" -eq 1 ]]; then continue; fi

      echo "Scraping page '$LNK'" 1>&2

      # Special cases
      if echo "$LNK" | grep 'ad\.co' > /dev/null
      then
        # shellcheck disable=SC2154
        "$olc" "$LNK" | grep -o "https://[^']*"
      fi

      # Generic scraper
      # shellcheck disable=SC2154
      "$scrapepage" "$LNK" || continue
    done < <(echo "$LINKS2")
  '';
}
