with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "warbo-utilities";
  src  = ./.;

  propagatedBuildInputs = [
    python
    md2pdf
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
