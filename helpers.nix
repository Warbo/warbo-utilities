{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "4a7075b";
    sha256 = "1263li5lc8fkhblzsbw1d7bchsnwprknk5xnajkkxcbikzczi3i4";
  };
}
