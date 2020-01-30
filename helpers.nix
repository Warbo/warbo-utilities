{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "2c50697";
    sha256 = "0gvxnv0k596arlqndlsgw3y5d2pxj263g2448vcqz25pf8x208dx";
  };
}
