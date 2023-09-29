{ bash, pkgs, wrap }:

wrap {
  name = "man";
  paths = [ pkgs.man ];
  script = ''
    #!${bash}/bin/bash
    if [[ "x$TERM" = "xdumb" ]]
    then
      # We're in Emacs, open this man page in Emacs's viewer
      emacsclient -e "(man \"$1\")"
    else
      # We're outside Emacs, use the normal man binary
      exec man "$@"
    fi
  '';
}
