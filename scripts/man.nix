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

  tests = builtins.attrValues {
    simpleTest = runCommand "man-simple-test" {} ''
      echo "Running simple test"
      true
      mkdir $out
    '';

    # Test scenario: Running outside of Emacs
    outsideEmacsTest = runCommand "man-outside-emacs-test" {} ''
      echo "Running outside Emacs test"
      # TODO: Add assertions to check if the standard 'man' command is executed
      true
      mkdir $out
    '';

    # Test scenario: Running inside Emacs
    insideEmacsTest = runCommand "man-inside-emacs-test" {} ''
      echo "Running inside Emacs test"
      # TODO: Add assertions to check if 'emacsclient' is executed
      true
      mkdir $out
    '';
  };
};
withDeps tests man
