with {
  # Unlike default.nix, we explicitly avoid loading system/user overlays, so
  # that the fallbacks defined in derivation.nix will be used.
  pkgs = import <nixpkgs> {
    config   = {};
    overlays = [ (import ./overlay.nix) ];
  };
};
{ inherit (pkgs) warbo-utilities warbo-utilities-scripts; }
