{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a15a328";
    sha256 = "03zp1sx18m244sgv0k0wp3zdp0pk9yf5yvhp08iwcwnqx9lc0gyd";
  };
}
