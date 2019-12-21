{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "2c70111";
    sha256 = "0wih2g4v7q84pjyas5a9n8klhgs504zqqnwhj6d0mbwa47dcqxzd";
  };
}
