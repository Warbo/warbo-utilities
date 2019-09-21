{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "080a590";
    sha256 = "0mnkshj0ygdxifhd9zdb6hfr7a406wny0slsy25xps39jwnvifzm";
  };
}
