with {
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
};
pkgs.warbo-utilities
