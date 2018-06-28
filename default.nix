with rec {
  nixpkgs = import ./nixpkgs.nix;
  pkgs    = import nixpkgs {
    args = { overlays = [ (import ./overlay.nix) ]; };
  };
};
pkgs.warbo-utilities
