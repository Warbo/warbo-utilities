with builtins;
with rec {
  # Old (< 2017) fetchgit gives different hashes, so allow override via NIX_PATH
  options =
    with tryEval <nix-config>;
    if success
       then { config = import "${value}/stable.nix"; f = "fetchGitHashless"; }
       else { config = {};                           f = "fetchgit";         };

  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  stableConfig =
    getAttr (options.f)
            (import <nixpkgs> { inherit (options) config; })
            {
              url    = http://chriswarbo.net/git/nix-config.git;
              rev    = "9f66c43";
              sha256 = "1cjca1l61dmpk2h5i7p8bj11dhcppbnj8hpjhzpjj16j2l121ih6";
            };

  # An awkward mix of unstable <nixpkgs> and stable nix-config. We only use this
  # to fetch other, purely stable/unstable package sets
  bootstrapPkgs = import <nixpkgs> {
    config = import "${stableConfig}/stable.nix";
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
                       while grep "extraFlagsArray" < "$script" > /dev/null
                       do
                         script=$(grep '^exec' < "$script" | cut -d ' ' -f2 |
                                                             tr -d '"')
                       done
                       echo "Checking '$script'" 1>&2

                       SHEBANG=$(head -n1 < "$script")
                       echo "$SHEBANG" | grep '^#!' > /dev/null || {
                         # Binaries, etc.
                         mkdir "$out"
                         exit 0
                       }

                       echo "$SHEBANG" | grep 'usr/bin/env' > /dev/null ||
                       echo "$SHEBANG" | grep '/nix/store'  > /dev/null ||
                         fail "Didn't use /usr/bin/env or /nix/store:\n$SHEBANG"

                       if echo "$SHEBANG" | grep 'bash' > /dev/null
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
   else { inherit scripts pkg nixPkgs; }
