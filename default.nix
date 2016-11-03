{ alsaUtils, bash, callPackage, coreutils, curl, gcalcli, gnugrep, gnused,
  jsbeautifier, lib, makeWrapper, pidgin, python, phantomjs, runCommand, stdenv,
  writeScript, xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with rec {
  scripts = rec {
    agenda    = callPackage ./scripts/agenda.nix    {};
    beeminder = callPackage ./scripts/beeminder.nix {};
    get_news  = callPackage ./scripts/get_news.nix  {};
    honk      = callPackage ./scripts/honk.nix      {};
    hot       = callPackage ./scripts/hot.nix       {};
    jovnc     = callPackage ./scripts/jovnc.nix     {};
    keys      = callPackage ./scripts/keys.nix      {};
    pinknoise = callPackage ./scripts/pinknoise.nix {};
  };

  mkCmd = name: script: ''cp "${script}" "$out/bin/${name}"'';
  cmds  = attrValues (mapAttrs mkCmd scripts);
};

stdenv.mkDerivation {
  name = "warbo-utilities";
  src  = ./.;

  propagatedBuildInputs = [
    python
    phantomjs
    jsbeautifier
    xidel
    xdotool
    xvfb_run
    xsel
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
