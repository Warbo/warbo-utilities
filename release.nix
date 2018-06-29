with rec {
  # Unlike default.nix, we explicitly avoid loading system/user overlays, so
  # that the fallbacks defined in derivation.nix will be used.
  pkgs = import <nixpkgs> { config = {}; overlays = []; };

  defs = pkgs.callPackage ./derivation.nix {};
};
{ warbo-utilities = defs.pkg; }
