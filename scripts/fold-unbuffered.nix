{ gcc, raw, runCommand }:

runCommand "build-fold-unbuffered"
  {
    src         = raw."fold.c";
    buildInputs = [ gcc ];
  }
  ''
    gcc "$src" -o "$out"
  ''
