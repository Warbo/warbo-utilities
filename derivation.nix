with rec {
  fetch   = args: import ((import <nixpkgs> { config = {}; }).fetchgit args);

  helpers = fetch {
    url    = http://chriswarbo.net/git/nix-helpers.git;
    rev    = "bb75beb";
    sha256 = "0cyp3drylrp0dvh4sll5lvi0n511grxfpmka2d5pdy4jxl0803p5";
  };

  packages = fetch {
    url    = http://chriswarbo.net/git/warbo-packages.git;
    rev    = "c9247be";
    sha256 = "099sk26lgh8zhfkm39zycsmr6vpr2mpx273lic04ckqcpfm4nzgr";
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
  extraArgs = bin // nix-helpers // warbo-packages // {
    raw = dirsToAttrs ./raw;

    # Force xidel version, to avoid argument incompatibilities
    inherit (nix-helpers.nixpkgs1709) xidel;
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
