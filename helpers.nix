{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "05182d0";
    sha256 = "1h8jrjrbbab0fymssdwgzl171x6lq817sjd8hzhia7kn5mgm2fib";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "9d26c29";
    sha256 = "1s9ih37xil0icdja7y5gv10sc7mbx92c529n5c5cx9r4m8mwzs4b";
  };
}
