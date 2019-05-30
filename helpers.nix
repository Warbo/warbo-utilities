{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "ed99cde";
    sha256 = "10vc4mg86cl1wv8gl5k5bvqyd7r6cbc1pj7kmlyj08zwa1wg0qrn";
  };
}
