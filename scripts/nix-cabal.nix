{ bash, cabal-install, cabal2nix, makeWrapper, runCommand, writeScript }:

runCommand "wrap-nix-cabal"
  {
    buildInputs = [ makeWrapper ];
    raw         = writeScript "nix-cabal" ''
      #!${bash}/bin/bash
      exec nix-shell -E "$(cabal2nix --shell ./.)" --run "cabal $@"
    '';
  }
  ''
    #!${bash}/bin/bash
    makeWrapper "$raw" "$out" \
      --prefix PATH : "${cabal-install}/bin" \
      --prefix PATH : "${cabal2nix}/bin"
  ''
