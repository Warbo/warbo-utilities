{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "1f239cb";
    sha256 = "12cr97xav0lshq8qmgn0b2dbmy8arjzknz2hq563zwdn7l1drvp3";
  };
}
