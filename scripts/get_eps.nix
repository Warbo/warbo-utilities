{ bash, cacert, coreutils, curl, fail, glibc, haskellPackages, html2text, raw
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
    file = raw."get_eps.sh";
    paths = [ bash coreutils curl glibc.bin wget ];
    vars = {
      inherit processor;
      SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    };
  };

  tests = {
    haveExpanse = runCommand "test-expanse" {
      inherit go;
      buildInputs = [ fail xidel ];
      KEEP_ALL = "1";
      file = fetchurl {
        name = "get_eps-test-expanse.html";
        url = "http://epguides.com/common/exportToCSVmaze.asp?maze=1825";
      };
    } ''
      CONTENT=$("$go" "TheExpanse" "$file") || fail "Failed to get eps"

      echo "$CONTENT" | xidel -s - -e '//item//pubDate' |
                        grep '2015-12-14' > /dev/null ||
        fail "Expanse s01e01 not found?\n$CONTENT"

      mkdir "$out"
    '';

    haveWalkingDead = runCommand "test-walking-dead" {
      inherit go;
      buildInputs = [ fail xidel ];
      KEEP_ALL = "1";
      file = builtins.fetchurl {
        name = "get_eps-test-walkingdead.html";
        url = "http://epguides.com/common/exportToCSVmaze.asp?maze=73";
      };
    } ''
      CONTENT=$("$go" "WalkingDead" "$file") || fail "Failed to get eps"

      echo "$CONTENT" | xidel -s - -e '//item//title' |
        grep 'What Comes After' > /dev/null ||
        fail "Walking Dead s09e05 not found?\n$CONTENT"

      mkdir "$out"
    '';
  };
};
withDeps [ (attrValues tests) ] go
