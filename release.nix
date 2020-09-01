with rec {
  inherit (import ./helpers.nix) nix-helpers sources warbo-packages;

  warbo-packages-sources = import "${sources.warbo-packages}/nix/sources.nix";

  pkgs = import nix-helpers.repoLatest {
    config   = {};
    overlays = [
      (import "${warbo-packages-sources.nix-helpers}/overlay.nix")
      (import "${sources.warbo-packages}/overlay.nix")
      (import ./overlay.nix)
    ];
  };
};
{
  inherit (pkgs) warbo-utilities warbo-utilities-scripts;
}
