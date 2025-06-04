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
    outsideEmacsUsesNormalMan = runCommand "man-outside-emacs-test" {
      buildInputs = [ man pkgs.grep ];
    } ''
      unset INSIDE_EMACS
      output=$("$man" --version 2>&1) || {
        echo "Error: Running man gave non-zero status '$?'"
        echo "Output:"
        echo "$output"
        exit 1
      } 1>&2

      # Exact output might vary, but should have "man-db" or "version"
      echo "$output" | grep -q "man-db\|version" || {
        echo "Error: Output doesn't look like real 'man' command"
        echo "Output:"
        echo "$output"
        exit 1
      } 1>&2

      mkdir "$out"
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
