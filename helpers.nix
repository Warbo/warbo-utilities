{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a9e7165";
    sha256 = "0bkn71apjw108grj0s0bpqnvxhgg29j2rm5khsa7pp05ir1g4r8m";
  };
}
