{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "c472887";
    sha256 = "0gjgimwhgg8q42bvswjnja1f1cg9akmi6j60s2czqlgqv15isjdj";
  };
}
