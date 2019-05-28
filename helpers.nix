{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5163dc8";
    sha256 = "05apxrrnfv1k630rm50gzyf8wq20qhcrpiy6ddb340r76r7g0fnp";
  };
}
