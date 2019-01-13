{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5f3c35c";
    sha256 = "02550v7h7rrvnbyjmhvs9wh03wdx0n89k8px43pi5qqn2gpcwnrg";
  };
}
