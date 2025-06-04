{
  bash,
  coreutils,
  emacs,
  gnugrep,
  pkgs,
  runCommand,
  withDeps,
  wrap,
  writeShellScript,
}:

with rec {
  man = wrap {
    name = "man";
    script = ''
      #!${bash}/bin/bash
      set -x
      REAL=${pkgs.man}/bin/man
      if [[ -n "''${INSIDE_EMACS:-}" ]]
      then
        # We're in Emacs, open this man page in Emacs's viewer
        # We can't override manual-program using let, since it's lexical
        emacsclient -e "(progn
                          (require 'man)
                          (let ((old manual-program))
                            (unwind-protect
                              (progn
                                (setq manual-program \"$REAL\")
                                (man \"$1\"))
                              (setq manual-program old))))"
      else
        # We're outside Emacs, use the normal man binary
        exec "$REAL" "$@"
      fi
    '';
  };

  tests = builtins.attrValues {
    outsideEmacsUsesNormalMan = runCommand "man-outside-emacs-test"
      { buildInputs = [ gnugrep ]; }
      ''
        unset INSIDE_EMACS
        output=$(${man} --version 2>&1) || {
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
    insideEmacsTest =
      with {
        testScript = writeShellScript "man-test-emacsclient.sh" ''
          set -e
          export HOME="$PWD" # Provide a home directory for Emacs
          emacs --daemon &
          EMACS_PID=$!
          trap "emacsclient -e '(kill-emacs)' || kill $EMACS_PID" EXIT

          printf "Waiting for Emacs daemon..." 1>&2
          for i in {1..10}
          do
            emacsclient -e t > /dev/null 2>&1 && {
              echo "Emacs daemon ready"
              break
            }
            printf '.'
            sleep 1
          done 1>&2
          emacsclient -e t > /dev/null 2>&2 || {
            echo "Error: Emacs daemon failed to start"
            exit 1
          } 1>&2

          # Simulate running inside Emacs and call the man script with 'ls'
          echo "Running man script inside simulated Emacs..." 1>&2
          # Run the man script, which calls emacsclient -e '(man "ls")'
          # Redirect its output to stderr or /dev/null to avoid interfering with the buffer check
          INSIDE_EMACS=t ${man} ls >&2 || { # Redirect stdout to stderr
            echo "Error: man script failed when run inside Emacs simulation" 1>&2
            exit 1
          }

          # Poll for the *Man ls* buffer
          echo "Polling for *Man ls* buffer..." 1>&2
          BUFFER_FOUND=0
          for i in {1..15}; do # Poll up to 15 times with 1-second delay
            # Capture output of get-buffer, discard its stderr
            buffer_output=$(emacsclient -e '(get-buffer "*Man ls*")' 2>/dev/null)
            if echo "$buffer_output" | ${gnugrep}/bin/grep -q "#<buffer"; then
              echo "*Man ls* buffer found after $i attempts." 1>&2
              BUFFER_FOUND=1
              break
            fi
            printf '.' 1>&2 # Print a dot for each attempt
            sleep 1
          done 1>&2 # Redirect printf output to stderr

          if [ "$BUFFER_FOUND" -eq 0 ]; then
            echo "Error: *Man ls* buffer not found after timeout." 1>&2
            echo "Last emacsclient output for get-buffer: '$buffer_output'" 1>&2
            exit 1
          fi

          echo "Inside Emacs test passed." 1>&2
          mkdir "$out" # Create the output directory to mark success
        '';
      };
      runCommand "man-inside-emacs-test"
        {
          buildInputs = [ coreutils emacs gnugrep ];
        }
        ''${testScript} && mkdir "$out"''; # Execute the script and create output dir
  };
};
withDeps tests man
