{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "7e73876";
    sha256 = "0zpr8pjf4k4g27fml97axj696c5wj4fd4jbdplrf94bas2ix0w1h";
  };
}
