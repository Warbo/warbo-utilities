{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "975c8a9";
    sha256 = "0dh65z483ddc770rvbnan65sizpw6gpz9lai6s3b34a2wlrl88r1";
  };
}
