{ expect, makeWrapper, runCommand, writeScript }:

runCommand "onealluc"
  {
    buildInputs = [ makeWrapper ];
    raw = writeScript "onealluc" ''
      #!/usr/bin/env expect
      # Default timeout is 10 seconds; -1 disables timeout
      set timeout -1

      # Run getalluc with all args, like bash $@
      set arguments [lrange $argv 0 end]
      spawn getalluc {*}$arguments

      # Quit after the first "inDir ..."
      expect {
        eof {
          send_user "\nNo results\n"
          exit 0
        }
        "inDir" {
          exit 0
        }
      }
      close
    '';
  }
  ''
    makeWrapper "$raw" "$out" --prefix PATH : "${expect}/bin"
  ''
