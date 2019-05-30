self: super:

with builtins;
with self.lib;
with rec {
  # Let scripts depend on each other by adding 'bin' to the argument set
  extraArgs = {
    raw     = self.dirsToAttrs ./raw;
    scripts = self.warbo-utilities-scripts;

    # Force xidel version, to avoid argument incompatibilities
    inherit (self.nixpkgs1709) xidel;
  };

  scripts = mapAttrs (_: f: self.newScope extraArgs f {})
                     (self.nixFilesIn ./scripts);

  cmds = foldl (rest: dir: rest // mapAttrs (f: _: dir + "/${f}")
                                            (readDir dir))
               {}
               [ ./system ./web ./git ./development ./docs ];

  check = mapAttrs (name: script: self.runCommand "check-${name}"
                     {
                       inherit script;
                       buildInputs = [ self.fail
                                       self.haskellPackages.ShellCheck ];
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
                   self.warbo-utilities-scripts;
};
{
  warbo-utilities-scripts = cmds // scripts;

  warbo-utilities = self.withDeps (attrValues check)
    (self.runCommand "warbo-utilities"
      {
        bin         = self.attrsToDirs (self.warbo-utilities-scripts // {
                        # Things we've not defined, but want to provide anyway
                        inherit (self) fail;
                      });
        buildInputs = [ self.makeWrapper ];
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
