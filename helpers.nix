{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "4a34488";
    sha256 = "09zlykkhlhqgz8rx7pa7h3nicni9ialn27l4n759rlqlabdxwp06";
  };
}
