{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a9661bc";
    sha256 = "0d0imn71k54bgxmg2j1pyr68mddqjmk6fb4nad1w2552hfnxqwlx";
  };
}
