{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "559d9af";
    sha256 = "0gsj236q8c97m8m0ah5pbbc2zy0jk2fi6kirm4dsg7n7yap9gvfz";
  };
}
