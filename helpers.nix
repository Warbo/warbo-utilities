{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9eba9ae";
    sha256 = "1dggw36yjmnd8gslynllwann4ixmvwcl62rk5b78s3gp245ws59j";
  };
}
