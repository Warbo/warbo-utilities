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
    outsideEmacsTest = runCommand "man-outside-emacs-test" {
      buildInputs = [ pkgs.grep ]; # Add grep to check output
    } ''
      echo "Running outside Emacs test" 1>&2

      # Ensure INSIDE_EMACS is not set
      unset INSIDE_EMACS

      # Run the man script with --version and capture output
      # Use the script itself, which is available via inherit man;
      output=$("$man" --version 2>&1)
      exit_code=$?

      # Check the exit code
      if [ "$exit_code" -ne 0 ]; then
        echo "Error: man script exited with non-zero status $exit_code" 1>&2
        echo "Output:" 1>&2
        echo "$output" 1>&2
        exit 1
      fi

      # Check if the output contains expected version information
      # The exact output might vary, but "man-db" or "version" is common
      if ! echo "$output" | ${pkgs.grep}/bin/grep -q "man-db\|version"; then
        echo "Error: Output does not contain expected version info" 1>&2
        echo "Output:" 1>&2
        echo "$output" 1>&2
        exit 1
      fi

      echo "Outside Emacs test passed: man --version executed correctly." 1>&2
      mkdir "$out" # Create the output directory to mark success
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
