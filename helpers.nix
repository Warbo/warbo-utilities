{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "aa8744e";
    sha256 = "0hri55f0is6ffbrldpskj6vh2i9ifkbcdq76b8kqp7app742bc8x";
  };
}
