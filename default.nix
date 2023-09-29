{ helpers ? import ./helpers.nix, nix-helpers ? warbo-packages.nix-helpers
, sources ? helpers.sources, warbo-packages ? helpers.warbo-packages }:

with rec {
  warbo-packages-sources = import "${sources.warbo-packages}/nix/sources.nix";

  pkgs = import nix-helpers.repoLatest {
    config = { };
    overlays = [
      (import "${warbo-packages-sources.nix-helpers}/overlay.nix")
      (import "${sources.warbo-packages}/overlay.nix")
      (import ./overlay.nix)
    ];
  };
};
pkgs.warbo-utilities
