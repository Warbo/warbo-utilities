with builtins;
with rec {
  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  stableConfig = rec {
    rev = "9f66c431de0d324b3f2507bea4dca5f3c2574a0e";
    url = "https://github.com/Warbo/nix-config/archive/${rev}.tar.gz";
  };

  config =
    with tryEval <nix-config>;
    if success
       then (import <nixpkgs> {
              config = import "${value}/stable.nix";
            }).latestGit {
              url = http://chriswarbo.net/git/nix-config.git;
              stable = {
                rev    = stableConfig.rev;
                sha256 = "1x8340ns235gy76zrhf63v7hcfhw1qv630zbd7aabbcsb294hf20";
              };
            }
       else fetchTarball stableConfig.url;

  # An awkward mix of unstable <nixpkgs> and stable nix-config. We only use this
  # to fetch other, purely stable/unstable package sets
  bootstrapPkgs = import <nixpkgs> {
    config = import "${config}/stable.nix";
  };

  # Uses stable config with stable nixpkgs
  stablePkgs = bootstrapPkgs.customised.nixpkgs1709 // {
    inherit (bootstrapPkgs.customised.nixpkgs1609) firefox;
  };
};

{
  # The nixpkgs set to use, e.g. if we want a particular revision. Should use
  # some version of nix-config, else our dependencies will be missing.
  nixPkgs ? stablePkgs,

  # If false, returns some of our intermediate results alongside the package
  packageOnly ? true
}:

with builtins;
with nixPkgs.lib;
with rec {
  inherit (nixPkgs) attrsToDirs fail haskellPackages newScope runCommand
                    withDeps;

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

  check = mapAttrs (name: script: runCommand "check-${name}"
                     {
                       inherit script;
                       buildInputs = [ fail haskellPackages.ShellCheck ];
                       LANG        = "en_US.UTF-8";
                     }
                     ''
                       set -e

                       # Unwrap until we get to the real implementation
                       while grep "extraFlagsArray" < "$script"
                       do
                         echo "Found wrapper '$script', unwrapping" 1>&2
                         script=$(grep '^exec' < "$script" | cut -d ' ' -f2 |
                                                             tr -d '"')
                       done
                       echo "Checking '$script'" 1>&2

                       SHEBANG=$(head -n1 < "$script")
                       echo "$SHEBANG" | grep '^#!' || {
                         # Binaries, etc.
                         mkdir "$out"
                         exit 0
                       }

                       echo "$SHEBANG" | grep 'usr/bin/env' ||
                       echo "$SHEBANG" | grep '/nix/store'  || {
                         fail "Didn't use /usr/bin/env or /nix/store:\n$SHEBANG"
                       }

                       if echo "$SHEBANG" | grep 'bash'
                       then
                         shellcheck "$script"
                       fi
                       mkdir "$out"
                     '')
                   bin;

  pkg = withDeps (attrValues check)
                 (attrsToDirs { inherit bin; });
};

if packageOnly
   then pkg
   else { inherit cmds scripts pkg nixPkgs; }
