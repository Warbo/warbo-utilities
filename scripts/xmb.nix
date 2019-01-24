{ bash, haskellPackages, mkBin, python, pythonPackages, tesseract, wrap }:

with rec {
  random_mail = mkBin {
    name   = "random_mail";
    script = ''
      #!/usr/bin/env bash

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
      #!/usr/bin/env bash
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
};
wrap {
  name   = "xmb";
  paths  = [ bash next python pythonPackages.lxml tesseract ];
  file   = "${haskellPackages.xmobar}/bin/xmobar";
}
