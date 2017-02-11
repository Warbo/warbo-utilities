{ bash, coreutils, firefox, jsbeautifier, lib, makeWrapper, phantomjs,
  procps, runCommand, utillinux, writeScript, xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with (rec {
  wrapIn = pkgs: s:
    let args = concatMap (pkg: ["--prefix" "PATH" ":" "${pkg}/bin"]) pkgs;
     in runCommand "${s.name}-wrapped" { buildInputs = [ makeWrapper ]; } ''
          #!${bash}/bin/bash
          makeWrapper "${s}" "$out" ${toString args}
        '';

  phantom_save_page = writeScript "phantom_save_page.js" ''
    // Use PhantomJS to load the URL given as a commandline argument, and write
    // the HTML to stdout. We wait for the page to finish loading before writing
    // it out, which means we see the result of any on-page-load Javascript,
    // which we wouldn't get with something like wget or curl.

    var page   = require('webpage').create();
    var system = require('system');

    // Make ourselves look more like a browser than a scraper
    page.settings.userAgent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.71 Safari/537.36';
    page.viewportSize = { width: 1024, height: 768 };

    // Add an error handler, since sites may have dodgy code
    // Taken from http://stackoverflow.com/a/19538646/884682
    handler = function(msg, trace) {
      var msgStack = ['ERROR: ' + msg];
      if (trace && trace.length) {
        msgStack.push('TRACE:');
        trace.forEach(function(t) {
          msgStack.push(' -> ' + t.file + ': ' + t.line + (t.function ? ' (in function "' + t.function + '")' : ""));
        });
      }

      system.stderr.write(msgStack.join('\n'));
    };

    page.onError    = handler;
    phantom.onError = handler;

    page.open(system.args[1], function() {
      page.evaluate(function() {
      });
    });

    page.onLoadFinished = function() {
      setTimeout(function() {
        system.stdout.write(page.content);
        phantom.exit(0);
      }, 2000);
    };
  '';

  download_page = wrapIn [phantomjs] (writeScript "download_page" ''
    #!${bash}/bin/bash

    [[ -n "$1" ]] || {
      echo "download_page needs a URL as argument" 1>&2
      exit 1
    }

    phantomjs "${phantom_save_page}" "$1"
  '');

  ff = wrapIn [coreutils firefox xdotool xsel] (writeScript "ff.sh" ''
         #!${bash}/bin/bash

         FF_DIR=$(mktemp -d -t 'ff.sh-XXXXX')

         echo "Opening Firefox on '$URL'" 1>&2
         timeout 60 firefox -safe-mode         \
                            -profile "$FF_DIR" \
                            -no-remote         \
                            -new-instance      \
                            "$URL" 1>&2 &
         FF_PID="$!"
         sleep 10

         echo "Skipping safe mode prompt" 1>&2
         xdotool key --clearmodifiers Return
         sleep 15

         echo "Opening Web console" 1>&2
         xdotool key ctrl+shift+K
         sleep 10

         echo "Extracting body HTML" 1>&2

         # shellcheck disable=SC2016
         xdotool type 'window.prompt("Copy to clipboard: Ctrl+C, Enter", document.body.innerHTML);'

         sleep 5
         xdotool key --clearmodifiers Return
         sleep 5

         echo "Copying content" 1>&2
         xdotool key ctrl+c
         sleep 5

         echo "Pasting content" 1>&2
         xsel --clipboard
         echo ""

         kill "$FF_PID"
         rm -rf "$FF_DIR"
       '');

  xvfbrunsafe = wrapIn [utillinux xvfb_run]
    (let wrap = s: "$" + "{" + s + "}";
      in writeScript "xvfb-run-safe" ''
           #!${bash}/bin/bash

           # allow settings to be updated via environment
           : "${wrap "xvfb_lockdir:=/tmp/xvfb-locks"}"
           : "${wrap "xvfb_display_min:=99"}"
           : "${wrap "xvfb_display_max:=599"}"

           mkdir -p -- "$xvfb_lockdir" || exit

           i="$xvfb_display_min"     # minimum display number
           while (( i < xvfb_display_max ))
           do
             if [ -f "/tmp/.X$i-lock" ]
             then
               # still avoid an obvious open display
               (( ++i ))
               continue
             fi

             # open a lockfile
             exec 5>"$xvfb_lockdir/$i" || continue

             # try to lock it
             if flock -x -n 5
             then
               # if locked, run xvfb-run
               exec xvfb-run --server-num="$i" "$@" || exit
             fi
             (( i++ ))
           done
         '');

  scrapepage = wrapIn [jsbeautifier] (writeScript "scrapepage" ''
    #!${bash}/bin/bash

    function findurl {
      # Try to narrow-down output lines to just URLs
      grep -o 'http[^ "<>]*' | grep -o '[^"]*' | grep -v 'ads.ad-center.com'
    }

    function getvid {
      # Look for video URLs
      INCOMING=$(cat)
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

    CONTENT=$(curl -s "$1")
    echo "$CONTENT" | getvid
    echo "$CONTENT" | geteval
  '');

  vidsfrompage = wrapIn [procps xidel] (writeScript "vidsfrompage" ''
    #!${bash}/bin/bash
    #! nix-shell -i bash -p xidel -p jsbeautifier -p xvfb_run -p xdotool -p xsel

    function scrapeWithFirefox {
      if pgrep -f ".*firefox.*" | grep -v conkeror | grep fox > /dev/null
      then
        echo "Can't scrape pages while Firefox is open, aborting." 1>&2
        exit 1
      fi

      while read -r LINE
      do
        if echo "$LINE" | grep "^http" > /dev/null
        then
          echo "$LINE"
        else
          echo "$LINE" 1>&2
        fi
      done < <(URL="$1" "${xvfbrunsafe}" "${ff}" | readUrls)
    }

    function readUrls {
      xidel - -q -e '//iframe/@src'
    }

    # Loop over result links, getting videos and obfuscated javascript
    while read -r LNK
    do
      echo "Scraping page '$LNK'" 1>&2
      "${scrapepage}" "$LNK"
    done < <(scrapeWithFirefox "$1")
  '');
});

wrapIn [xidel] (writeScript "getalluc" ''
  #!${bash}/bin/bash

  [[ -z "$DEBUG" ]] || set -x

  export SITE="http://www.alluc.ee"

  function search {
    # Poor man's URL escaping
    echo "$@" | sed -e 's/ /+/g'
  }

  # Search for commandline arguments and get videos
  Q=$(search "$@")
  URL="$SITE/stream/$Q"

  function runCurl {
    echo "Downloading '$URL'" 1>&2
    "${download_page}" "$URL"
  }

  function allResults {
    xidel - -q --extract '//div[@class="title"]/a/@href'
  }

  function removeAds {
    grep -v "^/source/"
  }

  function prefixLinks {
    while read -r REL
    do
      echo "$SITE""$REL"
    done
  }

  while read -r LINK
  do
    echo "Getting vids from page '$LINK'" 1>&2
    URLS=$("${vidsfrompage}" "$LINK")

    while read -r URL
    do
      [[ -n "$URL" ]] || continue

      echo "Got URL '$URL'" 1>&2
      FIXED=$(echo "$URL" | sed -e "s/'$//g")
      echo "inDir ~/Public/TODO wget -O '$*' '$FIXED'"

      [[ -z "$STOPONFIRST" ]] || exit
    done < <(echo "$URLS")
  done < <(runCurl | allResults | removeAds | prefixLinks)
'')
