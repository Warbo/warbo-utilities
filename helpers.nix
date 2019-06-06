{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "fa9bac6";
    sha256 = "0b1d69ray9hp4b0d8jwrph3rgkwkhmr8pkfpb4sjlvxcyjgc5nn9";
  };
}
