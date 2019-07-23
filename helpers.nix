{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "f0a0ce0";
    sha256 = "1c0zqpvi758p5vl031cv23iwr9z309n3rlw9nc06iwrxp2wwdi03";
  };
}
