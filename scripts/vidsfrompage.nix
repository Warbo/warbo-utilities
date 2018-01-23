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

      function findurl {
        # Try to narrow-down output lines to just URLs
        grep -o 'http[^ "<>]*' | grep -o '[^"]*' | grep -v 'ads.ad-center.com'
      }

      function getvid {
        # Look for video URLs
        INCOMING=$(cat)
        echo "$INCOMING" | xidel -q - -e '//video/@src'
        echo "$INCOMING" | getfmt mp4 | findurl
        echo "$INCOMING" | getfmt flv | findurl
        echo "$INCOMING" | getfmt avi | findurl
      }

      function geteval {
        # Send any obfuscated javascript through js-beautify, then look for vids
        EVALS=$(grep "eval" | sed -e 's/<[^>]*>//g')
        while read -r LINE
        do
          echo "$LINE" | js-beautify -i | getvid
        done < <(echo "$EVALS")
      }

      function getfmt {
        # Look for URLs of a particular video format
        INPUT=$(cat)
        while read -r CANDIDATE
        do
          # If we find '"foo.$1"' then use that, otherwise use the whole line
          if ! echo "$CANDIDATE" | grep -io "\"[^\"]*\.$1[^\"]*\""
          then
            echo "$CANDIDATE"
          fi
        done < <(echo "$INPUT" | grep -i "\.$1")
      }

      CONTENT1=$(curl -s "$1")

      echo "Scraping '$1' with Firefox" 1>&2

      # shellcheck disable=SC2154
      CONTENT2=$("$ff" "$1")
      CONTENT=$(echo -e "$CONTENT1\n$CONTENT2")
      echo "$CONTENT" | getvid
      echo "$CONTENT" | geteval
    '';
  };
};

wrap {
  name   = "vidsfrompage";
  paths  = [ bash procps wget xidel ];
  vars   = { inherit ff olc scrapepage; };
  script = ''
    #!/usr/bin/env bash

    function debugDump {
      if [[ -n "$DEBUG" ]]
      then
        echo "Source code dump:" 1>&2
        tee >(cat 1>&2)
        echo "End source code"   1>&2
      else
        cat
      fi
    }

    function getWithFirefox {
      echo "Looking for videos on '$1' with Firefox" 1>&2

      # shellcheck disable=SC2154
      "$ff" "$1" | debugDump | xidel - -q -e '//iframe/@src'
    }

    function scrapeWithFirefox {
      while read -r LINE
      do
        if echo "$LINE" | grep "^http" > /dev/null
        then
          echo "$LINE"
        else
          echo "$LINE" 1>&2
        fi
      done < <(getWithFirefox "$1")
    }

    function skipUrl {
      for PAT in 'recaptcha'
      do
        if echo "$1" | grep "$PAT" > /dev/null
        then
          return 0
        fi
      done
      return 1
    }

    # Look for raw URLs
    wget -q -O- "$1" | grep -o 'http[^ "]*mp4'

    # Loop over result links, getting videos and obfuscated javascript
    while read -r LNK
    do
      if skipUrl "$LNK"; then continue; fi
      echo "Scraping page '$LNK'" 1>&2

      # Special cases
      if echo "$LNK" | grep 'ad\.co' > /dev/null
      then
        # shellcheck disable=SC2154
        "$olc" "$LNK" | grep -o "https://[^']*"
      fi

      # Generic scraper
      # shellcheck disable=SC2154
      "$scrapepage" "$LNK"
    done < <(scrapeWithFirefox "$1")
  '';
}
