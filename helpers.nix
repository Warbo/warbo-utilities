{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "7f1abe0";
    sha256 = "1n36bl1302p56a9i6885klsgl5pjg335p1a5m8gxfnb3941l16jv";
  };
}
