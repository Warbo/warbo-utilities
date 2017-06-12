{ makeWrapper, nopriv, runCommand, writeScript }:

with rec {
  # We need to provide IMAP details, but they won't be used in offline mode
  ini = writeScript "nopriv.ini" ''
    [nopriv]
    imap_server   = example.com
    imap_user     = xyz@example.com
    imap_password = example
    imap_folder   = NoPriv_All
    offline       = true
  '';

  raw = writeScript "maildir2html-raw" ''
    #!/usr/bin/env bash
    nopriv
  '';

  homeDir = runCommand "nopriv-home"
    {
      inherit ini;
    }
    ''
      mkdir -p  "$out/.config"
      cp "$ini" "$out/.config/nopriv.ini"
    '';
};

runCommand "maildir2html"
  {
    inherit homeDir ini nopriv;
    buildInputs = [ makeWrapper ];
  }
  ''
    makeWrapper "$nopriv/bin/nopriv" "$out" --set HOME "$homeDir"
  ''
