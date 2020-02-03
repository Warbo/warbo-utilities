{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "17bb06b";
    sha256 = "1fwpv5d4s5nzgnhc944q9y139ljwd9ldfajday3n2ik7ckkijsb1";
  };
}
