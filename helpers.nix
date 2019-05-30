{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "92b0f2e";
    sha256 = "0s5k3z9737hxvl9c15zww1zsd8rhv4g72lvip0sb9rs5xyc495iw";
  };
}
