{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "762452e";
    sha256 = "1cw5g2ip2lr3mq2png216v7x7kkp4m3ypnbj992cb98mkv4sa1fr";
  };
}
