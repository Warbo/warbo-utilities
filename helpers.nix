{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b6a41c8";
    sha256 = "1b3jm09rbcj1slyiy4h3jbbw31c342880dzv30ka3vzkicd6ba4a";
  };
}
