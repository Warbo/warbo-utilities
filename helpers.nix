{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "1449c81";
    sha256 = "1mp04isrkmslldc6q0fqmckvjnf2rrm0r16nj96sxck87jadaa1n";
  };
}
