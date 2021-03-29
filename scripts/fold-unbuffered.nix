{ gcc, gnulib, raw, runCommand }:

runCommand "build-fold-unbuffered"
  {
    src         = raw."fold.c";
    buildInputs = [ gcc gnulib ];
  }
  ''
    gcc "$src" -o "$out"
  ''
