{ bash, coreutils, firefox, jsbeautifier, lib, makeWrapper, procps, runCommand,
  utillinux, writeScript, xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with rec {
  log = msg: ''if [[ -n "$DEBUG" ]]; then echo -e "${msg}"; fi'';

  wrapIn = pkgs: s:
    let args = concatMap (pkg: ["--prefix" "PATH" ":" "${pkg}/bin"]) pkgs;
     in runCommand "${s.name}-wrapped" { buildInputs = [ makeWrapper ]; } ''
          #!${bash}/bin/bash
          makeWrapper "${s}" "$out" ${toString args}
        '';

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

    if echo "$URL" | grep "ad.co/" > /dev/null
    then
      echo "Clicking" 1>&2
      xdotool mousemove 200 200
      sleep 2
      xdotool click 1
      sleep 10
    fi

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
    CONTENT2=$(URL="$1" "${xvfbrunsafe}" "${ff}")
    CONTENT=$(echo -e "$CONTENT1\n$CONTENT2")
    echo "$CONTENT" | getvid
    echo "$CONTENT" | geteval
  '');
};

wrapIn [procps xdotool xidel] (writeScript "vidsfrompage" ''
  #!${bash}/bin/bash

  function getWithFirefox {
    if pgrep -f ".*firefox.*" | grep -v conkeror | grep fox > /dev/null
    then
      echo "Can't scrape pages while Firefox is open, aborting." 1>&2
      exit 1
    fi
    URL="$1" "${xvfbrunsafe}" "${ff}"
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
    "${scrapepage}" "$LNK"
  done < <(scrapeWithFirefox "$1")
'')
