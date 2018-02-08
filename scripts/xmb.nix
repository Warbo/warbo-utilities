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
      M=""
      [[ -n "$G" ]] || M="$D"
      [[ -n "$D" ]] || M="$G"
      [[ -n "$M" ]] || {
        if [[ "$(( RANDOM % 3 ))" -eq 0 ]]
        then
          M="$D"
        else
          M="$G"
        fi
      }

      if [[ -n "$M" ]]
      then
        grep '^Subject:' < "$M" | sed -e 's/^Subject: //g'
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
      NEXT=$(head -n1 < ~/.next)

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
