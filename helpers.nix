{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "600ab59";
    sha256 = "0a3j2r9rqlpr4g6iml7zbd2498qrj2dlh7vib6w8mpxig3270z1h";
  };
}
