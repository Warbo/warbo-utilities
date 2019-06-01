{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5b6623a";
    sha256 = "0ai4p5hz3n8h9dn6iqsdfb20s92mx1164yl53lwz4cbhx7kibdnc";
  };
}
