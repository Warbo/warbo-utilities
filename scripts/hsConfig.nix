{ bash, cabal2nix, makeWrapper, nix, stdenv, writeScript }:
with builtins;
with rec {
  expr = writeScript "hsConfigExpr.nix" ''
    with import <nixpkgs> {};
    { c2n }: (tincify (haskellPackages.callPackage c2n {}) {}).env
  '';

  raw  = writeScript "hsConfig" ''
    #!${bash}/bin/bash -p
    CMD="cabal configure -v --enable-tests --enable-benchmarks $*"
    nix-shell --show-trace --arg c2n "$(cabal2nix ./.)" -E "import ${expr}" \
              --run "$CMD"
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
