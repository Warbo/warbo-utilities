{ bash, mkBin, nixpkgs1609, python, pythonPackages, tesseract, wrap }:

with rec {
  random_mail = mkBin {
    name   = "random_mail";
    script = ''
      #!/usr/bin/env bash
      F=$(find ~/Mail/gmail/INBOX/new -type f | shuf | head -n1)
      grep '^Subject:' < "$F" | sed -e 's/^Subject: //g'
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
  file   = "${nixpkgs1609.haskellPackages.xmobar}/bin/xmobar";
}
