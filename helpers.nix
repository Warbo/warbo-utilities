{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "70b1e7a";
    sha256 = "09i231qxavjy501i7yqiv2k8n62ya35wk035misvmc20b4dyqrms";
  };
}
