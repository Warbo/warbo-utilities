{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "92af319";
    sha256 = "09g8gx2cn3ak8dl688nqnvnb2c5x7y6jqcmr62gyy3rg3xm4v79f";
  };
}
