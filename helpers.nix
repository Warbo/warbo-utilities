{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "49e7fec";
    sha256 = "19yrn9mkkda24ws1isbf9ms1ppy910flmb6rccssqfnjfk3wpw4x";
  };
}
