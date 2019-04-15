{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "585a7e2";
    sha256 = "00y4ikvbs8kcrx96vl6l5szrdjkzc1j1ay184s50gkyai9b2gapq";
  };
}
