{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "aa5898b";
    sha256 = "0bvy31px99rk3rppxxjmaw7c4fmfk0m8a6yr0v0ps293p4knxj5q";
  };
}
