{ bash, coreutils, firefox, jsbeautifier, lib, makeWrapper, procps, runCommand,
  utillinux, wrap, writeScript, xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with rec {
  log = msg: ''if [[ -n "$DEBUG" ]]; then echo -e "${msg}"; fi'';

  scrapepage = wrap {
    name   = "scrapepage";
    paths  = [ jsbeautifier ];
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
        # Send any obfuscated javascript through js-beautify, then look for videos
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
      CONTENT2=$(ff "$1")
      CONTENT=$(echo -e "$CONTENT1\n$CONTENT2")
      echo "$CONTENT" | getvid
      echo "$CONTENT" | geteval
    '';
  };
};

wrap {
  name   = "vidsfrompage";
  paths  = [ procps xdotool xidel ];
  script = ''
    #!/usr/bin/env bash

    function getWithFirefox {
      if pgrep -f ".*firefox.*" | grep -v conkeror | grep fox > /dev/null
      then
        echo "Can't scrape pages while Firefox is open, aborting." 1>&2
        exit 1
      fi
      ff "$1"
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
      done < <(getWithFirefox "$1" | xidel - -q -e '//iframe/@src')
    }

    # Loop over result links, getting videos and obfuscated javascript
    while read -r LNK
    do
      echo "Scraping page '$LNK'" 1>&2

      # Special cases
      if command -v olc 1> /dev/null 2> /dev/null &&
         echo "$LNK" | grep 'ad\.co' > /dev/null
      then
        olc "$LNK"
      fi

      # Generic scraper
      "${scrapepage}" "$LNK"
    done < <(scrapeWithFirefox "$1")
  '';
}
