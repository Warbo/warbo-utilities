{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b5b4cf8";
    sha256 = "1s38bra8xrsd1g1ysyyrqk7p5i4wvfikvyd9xx9421r9pqzg8xj9";
  };
}
