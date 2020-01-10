{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "ccddc2b";
    sha256 = "08j4mb65asxp3yr4grgahr0288vx4ij3didr23r8f15c6qh7agrh";
  };
}
