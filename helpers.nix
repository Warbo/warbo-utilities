{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "718697f";
    sha256 = "03bhgh2a77paxrdy55pvg5mf0xbc8f935pvczd3blxyhblnckmq5";
  };
}
