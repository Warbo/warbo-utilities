{
  bash,
  pkgs,
  runCommand,
  withDeps,
  wrap,
}:

with rec {
  man = wrap {
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
};

  tests = {
    simpleTest = runCommand "man-simple-test" {} ''
      echo "Running simple test"
      true
      mkdir $out
    '';
  };
};

withDeps tests man
