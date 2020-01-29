{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5e76f62";
    sha256 = "1h3mw7bhkalysm81mmj717qsj6bixis5k909skg6zbgzncka79g7";
  };
}
