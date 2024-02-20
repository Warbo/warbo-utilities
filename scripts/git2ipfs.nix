{ attrsToDirs, bash, ipfs, runCommand, wrap, writeScript }:

with rec {
  ipfsBin = wrap {
    name = "ipfsBin";
    script = ''
      #!${bash}/bin/bash
      if command -v ipfs > /dev/null
      then
        ipfs "$@"
      else
        echo "Can't find 'ipfs', using potentially incompatible fallback" 1>&2
        "${ipfs}/bin/ipfs" "$@"
      fi
    '';
  };
};
wrap {
  name = "git2ipfs";
  paths = [ (attrsToDirs { bin = { inherit ipfsBin; }; }) ];
  script = ''
    #!${bash}/bin/bash
    set -e
    [[ -n "$1" ]] || {
      echo "No repo given, aborting" 1>&2
      exit 1
    }

    NAME=$(basename "$1" .git)

    ipfsBin key list | grep -q -Fx "$NAME" || {
      echo "Couldn't find key for '$NAME', can't push" 1>&2
      exit 1
    }

    if [[ -n "$PAGES" ]]
    then
      echo "Using pages from '$PAGES'" 1>&2
    else
      echo "Generating pages" 1>&2
      PAGES=$(repoPath="$1" htmlInOut=1 inNixedDir genGitHtml)
      echo "Saved in $PAGES" 1>&2
    fi

    echo "Pushing to IPFS" 1>&2
    IPFSHASH=$(ipfsBin add -rHq "$PAGES" | tail -n1)

    echo "Hash is $IPFSHASH" 1>&2

    echo "Publishing to IPNS" 1>&2
    ipfsBin name publish -k "$NAME" "$IPFSHASH"
  '';
}
