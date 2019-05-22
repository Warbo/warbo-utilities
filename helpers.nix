{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "48d4563";
    sha256 = "122k4ag8c8b4ly7df57j4rihl7gghyb4sxl6y0fxyjgxda9rrgcg";
  };
}
