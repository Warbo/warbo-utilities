{ fetchgit ? (import <nixpkgs> { config = {}; overlays = []; }).fetchgit }:

{
  nix-helpers = fetchgit {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "148bd5e";
    sha256 = "0wywgdmv4gllarayhwf9p41pzrkvgs32shqrycv2yjkwz321w8wl";
  };

  warbo-packages = fetchgit {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "e988092";
    sha256 = "082ibmy2q9zvrm85bncm10v29rm53k25dwlgmqgmldfppprxcwja";
  };
}
