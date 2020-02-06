{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "499d01c";
    sha256 = "13w276nz8lkvj5hh1xdmnhhym8k1qg2q17yksll8db41vjvicvz6";
  };
}
