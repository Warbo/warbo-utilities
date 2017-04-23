{ pkgs ? import <nixpkgs> {} }:

with builtins;
with pkgs;
with lib;
with rec {
  scripts = mapAttrs' (f: _: {
                        name  = removeSuffix ".nix" f;
                        value = callPackage (./scripts + "/${f}") {};
                      })
                      (readDir ./scripts);

  mkCmd = name: script: ''cp "${script}" "$out/bin/${name}"'';
  cmds  = attrValues (mapAttrs mkCmd scripts);
};

stdenv.mkDerivation {
  name = "warbo-utilities";
  src  = ./.;

  buildInputs = [ shellcheck ];

  propagatedBuildInputs = [
    python
  ];

  installPhase = ''
    mkdir -p "$out/bin"
    for DIR in svn system web git development testing docs
    do
      cp "$DIR/"* "$out/bin/"
    done
    ${concatStringsSep "\n" cmds}
  '';
}
