{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "ec2de3f";
    sha256 = "0g4bkigbins74qsga6ajkb97c75lkp7rskdkzqqdms0csm0g97b4";
  };
}
