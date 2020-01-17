{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9725758";
    sha256 = "041sz2xrk85cnx7gi2jsy9lpd5sp72bfr8bq7dc5akr1k7lf4zyk";
  };
}
