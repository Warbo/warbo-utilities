{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5659a2a";
    sha256 = "0wiwwv55112xjpm0j99ljk9ba38jgg7klark1ylp0a1arl1f35ir";
  };
}
