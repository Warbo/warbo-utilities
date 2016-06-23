with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "warbo-utilities";
  src  = ./.;

  propagatedBuildInputs = [
    python
  ];

  buildPhase   = "";
  installPhase = ''
    mkdir -p "$out/bin"
    for DIR in svn system web git development testing
    do
        cp "$DIR/"* "$out/bin/"
    done
  '';
}
