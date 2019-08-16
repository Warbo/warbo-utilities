{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "ea2e64e";
    sha256 = "1kz0d567m45745fzcp3flbhkcr05diig67scdhsdq3qxm9sh502s";
  };
}
