{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9cb537e";
    sha256 = "1df0453bq1ckj1cws3mwjsszpvmv1syxrqwkhnj5if7zs39618fs";
  };
}
