{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "53c38e4";
    sha256 = "0yp2f5gxd5dr11sg20zwhd7m3rf6z284sfwb1bjxyi9n57fy09lh";
  };
}
