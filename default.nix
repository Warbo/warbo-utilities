with builtins;
with rec {
  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  stableConfig = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "d1b2b9b";
    sha256 = "1rsax2izq5083wlxssg0ch4bxkg2g1hm2v61vp8frg5v9q55rlgr";
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
