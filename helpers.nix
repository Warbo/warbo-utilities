{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "e325b63";
    sha256 = "1caa0747ss83sjpmbvsamsf9nibvjqhl9b94h7k1jxl7zkhj5x6z";
  };
}
