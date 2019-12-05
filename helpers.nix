{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

rec {
  inherit (import "${warbo-packages}/helpers.nix" { inherit fetchgit; })
    nix-helpers;

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9ba9675";
    sha256 = "0mlcjji4hk46jf561k46kw5w0b6dwaigg77gg48qsxyw22267x4f";
  };
}
