{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "6884dc9";
    sha256 = "0vchy16bbg3sj0b4yarzbh56ynl8l3prwfijhrax4vznizwnfbgw";
  };
}
