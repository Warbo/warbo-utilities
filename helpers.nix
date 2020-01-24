{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9d38eb7";
    sha256 = "12q0kha3w9vh3kvlbrdjyxj345j33z5r189z8ay2ljpikkzq4hdg";
  };
}
