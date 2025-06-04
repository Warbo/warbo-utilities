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

          man_output=$(INSIDE_EMACS=t ${man} ls >&2) || { # Redirect stdout to stderr
            echo "Error: man script failed. Output:"
            echo "$man_output"
            exit 1
          } 1>&2

          printf "Waiting for *Man ls* buffer..." 1>&2
          FOUND=0
          for i in {1..15}
          do
            got=$(emacsclient -e '(get-buffer "*Man ls*")' 2>/dev/null)
            echo "$got" | grep -q "#<buffer" && {
              echo "found"
              FOUND=1
              break
            }
            printf '.'
            sleep 1
          done 1>&2

          [[ "$FOUND" -eq 1 ]] || {
            echo "Error: *Man ls* buffer not found after timeout."
            echo "Output of 'man' was: '$man_output'"
            echo "Last emacsclient output for get-buffer: '$got'"
            exit 1
          } 1>&2
          true
        '';
      };
      runCommand "man-inside-emacs-test"
        {
          buildInputs = [ coreutils emacs gnugrep ];
        }
        ''${testScript} && mkdir "$out"'';
  };
};
withDeps tests man
