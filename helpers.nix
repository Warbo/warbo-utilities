{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "5cecd3f";
    sha256 = "0g4qjciim81wi2hqydmlkxcb1923gaxdln5qx5icyy3639ap6xq3";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "a60fe06";
    sha256 = "155c0i1pqi98gkw7jsg7jz58x1zc9vy5494pvs633302qx29qkgr";
  };
}
