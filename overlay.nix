self: super:

with builtins;
with self.lib;
with rec {
  inherit (self.nix-helpers)
    attrsToDirs dirsToAttrs fail foldAttrs' nixFilesIn nixpkgs1709 patchShebang
    withDeps xvfb-run-safe;

  # Let scripts depend on each other by adding 'bin' to the argument set
  extraArgs = {
    raw = mapAttrs (name: entry:
      if isAttrs entry then
        ./raw + "/${name}"
      else if elem name [ "alert.wav" "bbcExamplePage.html.gz" ] then
        entry
      else
        patchShebang {
          inherit name;
          file = entry;
        }) (dirsToAttrs ./raw);
    scripts = warbo-utilities-scripts;

    # Force xidel version, to avoid argument incompatibilities
    inherit (nixpkgs1709) xidel;
  };

  scripts =
    mapAttrs (_: f: self.newScope extraArgs f { }) (nixFilesIn ./scripts);

  cmds = foldl (rest: dir: rest // mapAttrs (f: _: dir + "/${f}") (readDir dir))
    { } [ ./system ./web ./git ./development ./docs ];

  check = mapAttrs (name: script:
    self.runCommand "check-${name}" {
      inherit script;
      buildInputs = [ fail self.shellcheck ];
      LANG = "en_US.UTF-8";
    } ''
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
    '') warbo-utilities-scripts;

  warbo-utilities-scripts = cmds // scripts;

  warbo-utilities = withDeps (attrValues check)
    (self.runCommand "warbo-utilities" {
      __noChroot = true; # To access files linked to by our deps
      bin = attrsToDirs warbo-utilities-scripts;
      buildInputs = [ fail self.makeWrapper ];
      forContext = foldAttrs' (_: val: str:
        substring 0 0 ''
          ${val} ${str}
        '') "" warbo-utilities-scripts;
    } ''
      echo "Tying the knot between scripts" 1>&2
      mkdir -p "$out/bin" || fail "Couldn't make '$out/bin'"
      for P in ${escapeShellArg fail}'/bin/fail'          \
               ${escapeShellArg xvfb-run-safe}'/bin/xvfb-run-safe' \
               "$bin"/*
      do
        F=$(readlink -f "$P") || fail "Couldn't readlink '$P'"
        N=$(basename    "$P") || fail "No basename for '$P'"
        cp    "$F" "$out/bin/$N" || fail "Copy failed for '$F'"
        chmod +x   "$out/bin/$N" || fail "Couldn't chmod for '$F'"
        wrapProgram "$out/bin/$N" --prefix PATH : "$out/bin" ||
          fail "Couldn't wrap '$P'"
      done
      echo "Finished wrapping scripts" 1>&2
    '') // {
      inherit cmds scripts;
    };
}; {
  inherit warbo-utilities warbo-utilities-scripts;
}
