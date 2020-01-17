{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a46a1df";
    sha256 = "0h84p5l9250dzkad44l542myga7lq4kk4dfm9bnnvzj7nxwpjqb0";
  };
}
