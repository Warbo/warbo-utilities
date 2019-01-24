{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "c8fc45b";
    sha256 = "1sjxs3r95pw997v34r46vgcdz8ln8w7wpn9znh86chhqvl365llv";
  };
}
