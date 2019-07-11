{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "450d81f";
    sha256 = "0wg3bdq1vyjiykm1xmrq67hn5mklnnjkmw8inq2f330gv7k9d0vr";
  };
}
