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
  # Calls a script file, like 'callPackage' but drawing arguments from the given
  # set. We include nixpkgs, as usual, but we also allow scripts to depend on
  # each other (but not themselves).
  callScript = name: nixPkgs.newScope (nixPkgs // bin)
                                      (./scripts + "/${name}.nix")
                                      {};

  scripts = mapAttrs' (f: _: rec {
                        name  = removeSuffix ".nix" f;
                        value = callScript name;
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
