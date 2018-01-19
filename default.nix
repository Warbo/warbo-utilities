with builtins;
with rec {
  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  stableConfig = (import <nixpkgs> { config = {}; }).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "9f66c43";
    sha256 = "1x8340ns235gy76zrhf63v7hcfhw1qv630zbd7aabbcsb294hf20";
  };

  # An awkward mix of unstable <nixpkgs> and stable nix-config. We only use this
  # to fetch other, purely stable/unstable package sets
  bootstrapPkgs = import <nixpkgs> {
    config = import "${stableConfig}/stable.nix";
  };

  # Uses stable config with stable nixpkgs (repo1609 is fixed-output)
  stablePkgs = import bootstrapPkgs.repo1609 {
    config = import "${stableConfig}/stable.nix";
  };
};

{
  # The nixpkgs set to use, e.g. if we want a particular revision. Should use
  # some version of nix-config, else our dependencies will be missing.
  nixPkgs ? stablePkgs,

  # If false, returns some of our intermediate results alongside the package
  packageOnly ? true
}:

with nixPkgs.lib;
with rec {
  # Let scripts depend on each other by adding 'bin' to the argument set
  scripts = mapAttrs' (f: _: rec {
                        name  = removeSuffix ".nix" f;
                        value = nixPkgs.newScope (nixPkgs // bin)
                                                 (./scripts + "/${f}")
                                                 {};
                      })
                      (readDir ./scripts);

  cmds = foldl (rest: dir: rest // mapAttrs (f: _: dir + "/${f}")
                                            (readDir dir))
               {}
               [ ./svn ./system ./web ./git ./development ./testing ./docs ];

  bin = cmds // scripts;

  pkg = nixPkgs.attrsToDirs { inherit bin; };
};

if packageOnly
   then pkg
   else { inherit cmds scripts pkg nixPkgs; }
