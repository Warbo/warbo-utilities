{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "07a7832";
    sha256 = "0vn2xa78cpp3nrmgcvmzz7a82xhbnb3zx956979nm4gfabfy7q2v";
  };
}
