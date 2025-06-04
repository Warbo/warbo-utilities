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
        # Use cl-letf to temporarily set manual-program to the Nix-provided man binary
        emacsclient -e "(cl-letf (((manual-program \"$REAL\"))) (man \"$1\"))"
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

          INSIDE_EMACS=t ${man} ls || {
            echo "Error: man script failed when run inside Emacs simulation"
            exit 1
          } 1>&2
          sleep 2 # Adjust if needed

          # Should output #<buffer *Man ls*> or similar if it exists, else nil
          echo "Looking for Man buffer" 1>&2
          if emacsclient -e '(get-buffer "*Man ls*")' | grep "#<buffer" 1>&2
          then
            echo "Buffer found." 1>&2
          else
            echo "Error: *Man ls* buffer not found." 1>&2
            exit 1
          fi
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
