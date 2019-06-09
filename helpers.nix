{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "c7f83b8";
    sha256 = "1cx2w518sxr4933dr548ichaljhcp0wvmbgyv3m56lmfk6fqdgzq";
  };
}
