with rec {
  fetch   = args: import ((import <nixpkgs> { config = {}; }).fetchgit args);

  helpers = fetch {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "dc68891";
    sha256 = "0c4zh9cz1db5y5c1kmnwgj0f3s5528xahhs509gprlhvgyqyp1mc";
  };

  packages = fetch {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "b2010d7";
    sha256 = "0c4zh9cz1db5y5c1kmnwgj0f3s5528xahhs509gprlhvgyqyp1mc";
  };
};

{
  haskellPackages,
  lib,
  makeWrapper,
  newScope,
  nix-helpers ? helpers,
  runCommand,
  warbo-packages ? packages
}:

with builtins;
with lib;
with {
  inherit (nix-helpers) attrsToDirs dirsToAttrs fail nixFilesIn withDeps;
};
rec {
  # Let scripts depend on each other by adding 'bin' to the argument set
  extraArgs = bin // {
    inherit nix-helpers;
    raw = dirsToAttrs ./raw;
  };

  scripts = mapAttrs (_: f: newScope extraArgs f {})
                     (nixFilesIn ./scripts);

  cmds = foldl (rest: dir: rest // mapAttrs (f: _: dir + "/${f}")
                                            (readDir dir))
               {}
               [ ./system ./web ./git ./development ./docs ];

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
                 (runCommand "warbo-utilities"
                   {
                     bin         = attrsToDirs bin;
                     buildInputs = [ makeWrapper ];
                   }
                   ''
                     echo "Tying the knot between scripts" 1>&2
                     mkdir -p "$out/bin"
                     for P in "$bin"/*
                     do
                       F=$(readlink -f "$P")
                       N=$(basename    "$P")
                       cp "$F"  "$out/bin/$N"
                       chmod +x "$out/bin/$N"
                       wrapProgram "$out/bin/$N" --prefix PATH : "$out/bin"
                     done
                   '');
}
