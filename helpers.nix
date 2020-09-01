rec {
  inherit (warbo-packages) nix-helpers;

  sources = import ./nix/sources.nix;

  warbo-packages = import sources.warbo-packages;
}
