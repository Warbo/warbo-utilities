{ attrsToDirs, ipfs, runCommand, wrap, writeScript }:

with rec {
  ipfsBin = writeScript "ipfsBin" ''
    #!/usr/bin/env bash
    if command -v ipfs > /dev/null
    then
      ipfs "$@"
    else
      echo "Cann't find 'ipfs', using potentially incompatible fallback" 1>&2
      "${ipfs}/bin/ipfs" "$@"
    fi
  '';

  nixExpr = writeScript "gen.nix" ''
    with import <nixpkgs> {};
    { repo }:
      runCommand "gen" { inherit repo; buildInputs = [ warbo-utilities ]; }
        '''
          mkdir "$out"
          genGitHtml "$repo" "$out"
        '''
  '';
};
wrap {
  paths  = [ (attrsToDirs { bin = { inherit ipfsBin; }; }) ];
  script = ''
    #!/usr/bin/env bash
    set -e
    [[ -n "$1" ]] || {
      echo "No repo given, aborting" 1>&2
      exit 1
    }

    NAME=$(basename "$1" .git)

    ipfsBin key list | grep -Fx "$NAME" > /dev/null || {
      echo "Couldn't find key for '$NAME', can't push" 1>&2
      exit 1
    }

    echo "Generating pages" 1>&2
    BASE=$(dirname "$(readlink -f "$0")")
    PAGES=$(nix-build --show-trace --no-out-link \
                      --argstr repo "$1" --argstr base "$BASE" \
                      -E 'import "${nixExpr}"')

    echo "Saved in $PAGES" 1>&2

    echo "Pushing to IPFS" 1>&2
    IPFSHASH=$(ipfsBin add -rHq "$PAGES" | tail -n1)

    echo "Hash is $IPFSHASH" 1>&2

    echo "Publishing to IPNS" 1>&2
    ipfsBin name publish -k "$NAME" "$IPFSHASH"
  '';
}
