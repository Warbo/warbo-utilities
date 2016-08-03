with import <nixpkgs> {};

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

  buildPhase   = "";
  installPhase = ''
    mkdir -p "$out/bin"
    for DIR in svn system web git development testing docs
    do
        cp "$DIR/"* "$out/bin/"
    done
  '';
}
