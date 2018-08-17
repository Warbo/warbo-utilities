{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "35797e2";
    sha256 = "0mj1q72sjbdalcvaqk3pk1ik9k1bgqmd5igv20ks2miwg5hr2bic";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "dc24001";
    sha256 = "1gb0d6v4k9vk83icnrzh4qai3g4wkpvd41awg76h78ih4cwx3zaf";
  };
}
