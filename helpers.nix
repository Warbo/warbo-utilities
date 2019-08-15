{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9df2e10";
    sha256 = "0mqqgdklflaflky4rdcfia1d6g30j87s1g7g5w2lmvif9bkcqdkz";
  };
}
