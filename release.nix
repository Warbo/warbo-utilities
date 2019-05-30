with rec {
  inherit (import ./helpers.nix {}) nix-helpers warbo-packages;

  pkgs = import <nixpkgs> {
    config   = {};
    overlays = [
      (import "${nix-helpers   }/overlay.nix")
      (import "${warbo-packages}/overlay.nix")
      (import ./overlay.nix)
    ];
  };
};
{ inherit (pkgs) warbo-utilities warbo-utilities-scripts; }
