{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "8205708";
    sha256 = "04ch47rxva07wmdy2mhg4afjrn2sxp29as5h5q1d3sqz782f6ach";
  };
}
