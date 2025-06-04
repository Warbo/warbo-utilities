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
    insideEmacsTest = runCommand "man-inside-emacs-test" {
      buildInputs = [
        man # The man script itself
        pkgs.emacs # Emacs for daemon and client
        pkgs.man # The real man binary (needed by Emacs's man function)
        pkgs.gnugrep # For checking emacsclient output
        pkgs.coreutils # For sleep, kill, etc.
      ];
    } ''
      echo "Running inside Emacs test" 1>&2

      # Start Emacs daemon in the background
      echo "Starting Emacs daemon..." 1>&2
      emacs --daemon &
      EMACS_PID=$!

      # Ensure the daemon is killed on exit
      trap "emacsclient -e '(kill-emacs)' || kill $EMACS_PID" EXIT

      # Wait for daemon to be ready
      echo "Waiting for Emacs daemon..." 1>&2
      for i in {1..10}; do
        if emacsclient -e t > /dev/null 2>&1; then
          echo "Emacs daemon ready." 1>&2
          break
        fi
        sleep 1
      done

      # Check if daemon started successfully
      if ! emacsclient -e t > /dev/null 2>&2; then
        echo "Error: Emacs daemon failed to start." 1>&2
        exit 1
      fi

      # Simulate running inside Emacs and call the man script with 'ls'
      echo "Running man script inside simulated Emacs..." 1>&2
      INSIDE_EMACS=t "$man" ls || {
        echo "Error: man script failed when run inside Emacs simulation." 1>&2
        exit 1
      } 1>&2

      # Wait a moment for Emacs to process the request and open the buffer
      sleep 2 # Adjust if needed

      # Check if the *Man ls* buffer exists in Emacs
      echo "Checking for *Man ls* buffer..." 1>&2
      # emacsclient -e '(get-buffer "*Man ls*")' will output something like #<buffer *Man ls*> if it exists, or nil otherwise.
      # We grep for "#<buffer" to confirm existence.
      if emacsclient -e '(get-buffer "*Man ls*")' | ${pkgs.gnugrep}/bin/grep -q "#<buffer"; then
        echo "*Man ls* buffer found." 1>&2
      else
        echo "Error: *Man ls* buffer not found." 1>&2
        exit 1
      fi

      echo "Inside Emacs test passed." 1>&2
      mkdir "$out" # Create the output directory to mark success
    '';
  };
};
withDeps tests man
