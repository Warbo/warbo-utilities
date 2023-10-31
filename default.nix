{ nix-helpers ? warbo-packages.nix-helpers, nixpkgs ? nix-helpers.nixpkgs
, nixpkgs-lib ? nix-helpers.nixpkgs-lib
, warbo-packages ? import ./warbo-packages.nix }:

with rec {
  inherit (builtins) attrValues elem isAttrs mapAttrs readDir substring;

  inherit (nixpkgs-lib) escapeShellArg foldl;

  inherit (nix-helpers)
    attrsToDirs dirsToAttrs fail foldAttrs' nixFilesIn nixpkgs1709 patchShebang
    withDeps;

  inherit (nixpkgs) bash makeWrapper newScope runCommand shellcheck;

  # Let scripts depend on each other by adding 'bin' to the argument set
  extraArgs = nix-helpers // warbo-packages // {
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
  };

  scripts = mapAttrs (_: f: newScope extraArgs f { }) (nixFilesIn ./scripts);

  cmds = foldl (rest: dir: rest // mapAttrs (f: _: dir + "/${f}") (readDir dir))
    { } [ ./system ./web ./git ./development ./docs ];

  check = mapAttrs (name: script:
    runCommand "check-${name}" {
      inherit script;
      buildInputs = [ fail shellcheck ];
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
};
withDeps (attrValues check) (runCommand "warbo-utilities" {
  bin = attrsToDirs warbo-utilities-scripts;
  buildInputs = [ fail makeWrapper ];
  forContext = foldAttrs' (_: val: str:
    substring 0 0 ''
      ${val} ${str}
    '') "" warbo-utilities-scripts;
} ''
  echo "Tying the knot between scripts" 1>&2
  mkdir -p "$out/bin" || fail "Couldn't make '$out/bin'"
  for F in ${fail}/bin/fail "$bin"/*
  do
    N=$(basename "$F") || fail "No basename for '$F'"
    # Create a trampoline, to avoid problems with symlinks, etc.
    {
      echo '#!${bash}/bin/bash'
      printf 'exec %s "$@"' "$F"
    } > "$out/bin/$N"
    chmod +x "$out/bin/$N"
    wrapProgram "$out/bin/$N" --prefix PATH : "$out/bin" ||
      fail "Couldn't wrap '$F'"
  done
  echo "Finished wrapping scripts" 1>&2
'') // {
  inherit cmds nix-helpers nixpkgs nixpkgs-lib scripts warbo-packages;
  warbo-utilities-src = ./.;
}
