{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "6227a40";
    sha256 = "077kcal167ixwjj41sqrndd5pwvyavs20h826qx3ijl2i02wmwxs";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b934330";
    sha256 = "0gp8fvf4nz0ylqma5q4pxs6580gb694bhwkh3dvh4fy1gmyn2xzz";
  };
}
