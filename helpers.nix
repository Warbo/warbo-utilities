{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "4590188";
    sha256 = "0dgm1949bksk4pv7kym678nbfzxizx68g8bzlvk9ylrl5w102a7h";
  };
}
