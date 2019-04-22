{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "edd0725";
    sha256 = "13yr9hzlb1r661m012ahzh2sqfkyllcqd1syd8dalmi0l1mglxlc";
  };
}
