# We need a few helpers and packages from nix-config, so default to a
# known-good version. Importing nix-config will import nixpkgs too.
(import <nixpkgs> { config = {}; }).fetchgit {
  url    = http://chriswarbo.net/git/nix-config.git;
  rev    = "ce03e5e";
  sha256 = "1qg4ihf5w7xzsk1cdba7kzdl34jmdzvaf7vr6x0r86zgxn0zc5yj";
}
