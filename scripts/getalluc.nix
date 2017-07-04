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

    # Avoid '.html' as it's often '.avi.html' and other such nonsense.
    # Avoid 'thevideo.me' since their URLs contain Rick Rolls!
    URLS=$(vidsfrompage "$LINK" | grep -v '\.html' | grep -v '\.thevideo\.me')

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
