{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "64172a7";
    sha256 = "1w20y6i3vd35xi454cqwjfiv2z2v2v41yz40xnqcswymikkdn75h";
  };
}
