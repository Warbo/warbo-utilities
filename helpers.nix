{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a3b7dc6";
    sha256 = "0nbbmbzabw4abizqg2qrwvd7khf04b96ilfsy8qrwi49dniz78zz";
  };
}
