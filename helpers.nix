{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "4ab7185";
    sha256 = "0lg6ydf5prlssihy9bkr4ssykf1cr09kzihjgmpwv94rlv8s84ky";
  };
}
