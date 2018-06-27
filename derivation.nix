{ attrsToDirs, fail, haskellPackages, lib, makeWrapper, newScope, runCommand,
  withDeps }:

with builtins;
with lib;
rec {
  # Let scripts depend on each other by adding 'bin' to the argument set
  scripts = mapAttrs' (f: _: rec {
                        name  = removeSuffix ".nix" f;
                        value = newScope (nixPkgs // bin)
                                         (./scripts + "/${f}")
                                         {};
                      })
                      (readDir ./scripts);

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
