{
  bash,
  pkgs,
  wrap,
}:

wrap {
  name = "man";
  script = ''
    #!${bash}/bin/bash
    REAL=${pkgs.man}/bin/man
    if [[ -n "$INSIDE_EMACS" ]]
    then
      # We're in Emacs, open this man page in Emacs's viewer
      emacsclient -e "(progn (require 'cl-lib) (cl-letf (((\"$REAL\" 'manual-program) (man \"$1\")))))"
    else
      # We're outside Emacs, use the normal man binary
      exec "$REAL" "$@"
    fi
  '';
}
