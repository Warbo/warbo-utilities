{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "4fb7c87";
    sha256 = "1wwagwxjj8v5q5wi75fcqz6xl17b1jf4l5p051vx371xj373v3dk";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "6e4a804";
    sha256 = "1aqp5qyvv47cpf16hyv5i41820hjymdbs3xr8czgd8hipjz601zz";
  };
}
