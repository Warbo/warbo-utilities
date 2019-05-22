{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "1d9ce7d";
    sha256 = "0viz492xf45md0wchfs82a5y8v5kx903b4yvi9lzdqpfdy4qg9qr";
  };
}
