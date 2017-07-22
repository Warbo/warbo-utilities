{ attrsToDirs, ipfs, runCommand, wrap, writeScript }:

with rec {
  ipfsBin = writeScript "ipfsBin" ''
    #!/usr/bin/env bash
    if command -v ipfs > /dev/null
    then
      ipfs "$@"
    else
      echo "Can't find 'ipfs', using potentially incompatible fallback" 1>&2
      "${ipfs}/bin/ipfs" "$@"
    fi
  '';

  # Builds repo pages using Nix, which handles temp dirs, caching, etc. for us
  nixExpr = writeScript "gen.nix" ''
    with import <nixpkgs> {};
    with builtins;
    { cmd, repo }:
      runCommand "gen" { inherit cmd currentTime repo; }
        '''
          mkdir "$out"
          "$cmd" "$repo" "$out"
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
     CMD=$(command -v genGitHtml)
    PAGES=$(nix-build --show-trace --no-out-link \
                      --argstr repo "$1" --argstr cmd "$CMD" \
                      -E 'import "${nixExpr}"')

    echo "Saved in $PAGES" 1>&2

    echo "Pushing to IPFS" 1>&2
    IPFSHASH=$(ipfsBin add -rHq "$PAGES" | tail -n1)

    echo "Hash is $IPFSHASH" 1>&2

    echo "Publishing to IPNS" 1>&2
    ipfsBin name publish -k "$NAME" "$IPFSHASH"
  '';
}
