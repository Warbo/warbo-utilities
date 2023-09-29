{ bash, coreutils, curl, fail, glibc, haskellPackages, html2text, raw
, runCommand, wget, withDeps, wrap, xidel }:

with builtins;
with rec {
  processor = runCommand "get-eps-processor" {
    buildInputs =
      [ (haskellPackages.ghcWithPackages (hs: [ hs.bytestring hs.time ])) ];
    script = raw."get_eps.hs";
  } ''
    cp "$script" ./Main.hs
    ghc --make -o Main Main.hs
    cp Main "$out"
  '';

  go = wrap {
    name = "get-eps";
    paths = [ bash coreutils curl glibc.bin wget ];
    vars = {
      inherit processor;
      SSL_CERT_FILE = /etc/ssl/certs/ca-bundle.crt;
    };
    script = ''
      #!${bash}/bin/bash
      set -e

      echo "$2" | grep 'epguides.com' > /dev/null ||
        fail 'get_eps URL should be from epguides.com'

      PAGE=$(curl -f "$2") || fail "Couldn't download '$2'"

      # shellcheck disable=SC2154
      echo "$PAGE" | FEED="$1" "$processor"
    '';
  };

  tests = {
    haveExpanse = runCommand "test-expanse" {
      inherit go;
      __noChroot = true;
      buildInputs = [ curl fail xidel ];
      KEEP_ALL = "1";
      URL = "http://epguides.com/common/exportToCSVmaze.asp?maze=1825";
    } ''
      curl -s http://epguides.com > /dev/null || {
        echo "WARNING: Couldn't access epguides (offline?). Skipping test" 1>&2
        mkdir "$out"
        exit
      }
      CONTENT=$("$go" "TheExpanse" "$URL") || fail "Failed to get eps"

      echo "$CONTENT" | xidel -q - -e '//item//pubDate' |
                        grep '2015-12-14' > /dev/null ||
        fail "Expanse s01e01 not found?\n$CONTENT"

      mkdir "$out"
    '';

    haveWalkingDead = runCommand "test-walking-dead" {
      inherit go;
      __noChroot = true;
      buildInputs = [ curl fail xidel ];
      KEEP_ALL = "1";
      URL = "http://epguides.com/common/exportToCSVmaze.asp?maze=73";
    } ''
      curl -s http://epguides.com > /dev/null || {
        echo "WARNING: Couldn't access epguides (offline?). Skipping test" 1>&2
        mkdir "$out"
        exit
      }
      CONTENT=$("$go" "WalkingDead" "$URL") || fail "Failed to get eps"

      echo "$CONTENT" | xidel -q - -e '//item//title' |
        grep 'What Comes After' > /dev/null ||
        fail "Walking Dead s09e05 not found?\n$CONTENT"

      mkdir "$out"
    '';
  };
};
withDeps [ (attrValues tests) ] go
