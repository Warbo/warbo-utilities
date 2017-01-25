{ bash, cabal2nix, makeWrapper, nix, stdenv, writeScript }:
with builtins;
with rec {
  raw = writeScript "hsConfig" ''
    #!${bash}/bin/bash -p
    CMD="cabal configure -v --enable-tests --enable-benchmarks $*"
    nix-shell --show-trace -E "$(cabal2nix --shell ./.)" --run "$CMD"
  '';
};
stdenv.mkDerivation {
  name         = "hsConfig";
  buildInputs  = [ makeWrapper ];
  buildCommand = ''
    set -e
    makeWrapper "${raw}" "$out" --prefix PATH :     "${cabal2nix}/bin" \
                                --prefix PATH :     "${nix.out}/bin"   \
                                --set    NIX_REMOTE "daemon"           \
                                --set    NIX_PATH   "${getEnv "NIX_PATH"}"
  '';
}
