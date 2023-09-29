rec {
  sources = import ./nix/sources.nix;

  warbo-packages = import sources.warbo-packages;
}
