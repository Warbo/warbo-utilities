{ alsaUtils, bash, jsbeautifier, lib, pidgin, python, phantomjs, stdenv,
  writeScript, xdotool, xidel, xsel, xvfb_run }:

with builtins;
with lib;
with rec {
  system = rec {
    honk = writeScript "honk" ''
      #!${bash}/bin/bash
      ${alsaUtils}/bin/amixer sset Master unmute > /dev/null
      ${alsaUtils}/bin/aplay ${pidgin}/share/sounds/purple/alert.wav > /dev/null 2>&1
    '';
  };

  mkCmd = name: script: ''cp "${script}" "$out/bin/${name}"'';
  cmds  = concatMap (set: attrValues (mapAttrs mkCmd set)) [ system ];
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
