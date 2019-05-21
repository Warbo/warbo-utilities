{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "756e1db";
    sha256 = "1ygk8qcs202w055mkhfw471diniygj37iya9vbh6xzzsrsiqazj1";
  };
}
