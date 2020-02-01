{ bash, haskell-nix, mkBin, python, pythonPackages, wrap }:

with rec {
  random_mail = mkBin {
    name   = "random_mail";
    script = ''
      #!${bash}/bin/bash

      # Look up one email from each inbox
      G=$(find ~/Mail/gmail/INBOX/new  -type f | shuf | head -n1)
      D=$(find ~/Mail/dundee/INBOX/new -type f | shuf | head -n1)

      # Pick one or the other
      SOURCE=""
      M=""
      [[ -n "$G" ]] || {
        M="$D"
        SOURCE="DD"
      }
      [[ -n "$D" ]] || {
        M="$G"
        SOURCE="GM"
      }
      [[ -n "$M" ]] || {
        if [[ "$(( RANDOM % 3 ))" -eq 0 ]]
        then
          M="$D"
          SOURCE="DD"
        else
          M="$G"
          SOURCE="GM"
        fi
      }

      # Extract and format content
      function getOne {
        grep "$1" | "$2" | grep '^.' | head -n1 | sed -e "$3" | cut -c1-"$4"
      }


      if [[ -n "$M" ]]
      then
        FROM=$(getOne '^From:'    "cat"  's/^From: //g'    15 < "$M")
        SUBJ=$(getOne '^Subject:' "cat"  's/^Subject: //g' 28 < "$M")
        LINE=$(getOne '^[^:<>]*$' "shuf" 's/  */ /g'       28 < "$M")
        echo "$SOURCE:$FROM|$SUBJ|$LINE"
      else
        echo "No mail?"
      fi
    '';
  };
  next = mkBin {
    name   = "next";
    paths  = [ random_mail ];
    script = ''
      #!${bash}/bin/bash
      MAIL=$(random_mail)
      NEXT=$(grep -v '^#' < ~/.next | grep '^.' | shuf | head -n1) || true

      [[ -n "$MAIL" ]] || {
        echo "$NEXT"
        exit 0
      }

      [[ -n "$NEXT" ]] || {
        echo "$MAIL"
        exit 0
      }

      if [[ "$(( RANDOM % 3 ))" -eq 0 ]]
      then
        echo "$NEXT"
      else
        echo "$MAIL"
      fi
      exit 0
    '';
  };

  xmobar =
    with { hn = haskell-nix {}; };
    (hn.haskell-nix.hackage-package {
      name        = "xmobar";
      version     = "0.30";
      ghc         = hn.buildPackages.pkgs.haskell-nix.compiler.ghc865;
      index-state = "2020-01-11T00:00:00Z";
      modules     = [
        # Without this we get 'error: The Nixpkgs package set does not contain
        # the package: Xrender (system dependency). You may need to augment the
        # system package mapping in haskell.nix so that it can be found.'
        # That's what we're doing here.
        ({ pkgs, ... }: {
          # Anything we add to _module.args.pkgs can be used as an argument by a
          # cabal-to-nix generated function. 'Xrender' is one such argument.
          _module.args.pkgs = { Xrender = pkgs.xorg.libXrender; };

          # The Haskell X11 library needs libXscrnSaver; since we're adding that
          # extra dependency, we might as well enable Xinerama support too.
          packages.X11.components.library.libs = [
            pkgs.xorg.libXinerama
            pkgs.xorg.libXScrnSaver
          ];
        })
      ];
    }).components.exes.xmobar;
};
wrap {
  name  = "xmb";
  paths = [ bash next python pythonPackages.lxml ];
  file  = "${xmobar}/bin/xmobar";
}
