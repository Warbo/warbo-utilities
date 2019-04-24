{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b23cb8e";
    sha256 = "00ffaqy0v9cf4kriyx7xqg6nsgjyxq4ys7md7bagbzwzpf9lkdpw";
  };
}
