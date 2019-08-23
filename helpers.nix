{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9baaca7";
    sha256 = "1g4s53wx6fddr8gpbz0789cq7961xj0kbxlqfl6z996c6532y6k0";
  };
}
