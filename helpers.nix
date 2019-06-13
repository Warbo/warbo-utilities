{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "3157719";
    sha256 = "1zayfrrnqjhl1l57mfb31pn943b2zvjgl6p3kvz6wslg0r0gd6wi";
  };
}
