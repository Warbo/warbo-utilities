{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "5ee2a73";
    sha256 = "16c1akav6rs7vn61392cm6npwlbxp3s4rc3wmyhyc0chswb3gd5f";
  };
}
