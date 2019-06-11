{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b28eeb7";
    sha256 = "1p2nnlmdwbqrnvqdfj07qaq28j5j6vzbqrw4yrxhm9vbiv0rrbm1";
  };
}
