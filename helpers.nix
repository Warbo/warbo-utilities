{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "6626cc0";
    sha256 = "1pdrxcq5r1pqyijd4216fgs3663wsi07js5cfq4bnh53qv687rkf";
  };
}
