{ nix-helpers ? null
, nixpkgs ? null
, nixpkgs-lib ? null
, warbo-packages ? null
, warbo-packages-tree
}:
with rec {
  inherit (builtins) currentSystem getEnv pathExists;

  resolved = {
    warbo-packages =
      if warbo-packages == null
      then import (fetchGitIPFS warbo-packages-tree)
        ((if nix-helpers == null then {} else { inherit nix-helpers; }) //
         (if nixpkgs == null then {} else { inherit nixpkgs; }) //
         (if nixpkgs-lib == null then {} else { inherit nixpkgs-lib; }))
      else warbo-packages;
    nix-helpers =
      if nix-helpers == null
      then resolved.warbo-packages.nix-helpers or null
      else nix-helpers;
    nixpkgs =
      if nixpkgs == null
      then resolved.warbo-packages.nixpkgs or
        (resolved.nix-helpers.nixpkgs or null)
      else nixpkgs;
    nixpkgs-lib =
      if nixpkgs-lib == null
      then resolved.warbo-packages.nixpkgs-lib or
        (resolved.nix-helpers.nixpkgs-lib or null)
      else nixpkgs-lib;
  };

  # A known version of fetchGitIPFS.nix, fetched from IPFS. Used to fetch a copy
  # of nix-helpers if we've not been given one already; once we have nix-helpers
  # we can use its fetchGitIPFS instead.
  boot_fetchGitIPFS =
    with rec {
      # The version of fetchGitIPFS.nix. Shouldn't need updating often.
      cid = "bafkreihec2oflgbudu5ncpttdyqsjcc74hs4k26b6absfdmqopqogajhj4";
      narHash = "sha256-CgpPlgMjIt3DYVTJYmpdm/V5626f2s5lrEMtGEYQzTU=";

      # fetchTree only takes one URL, so allow it to be overridden by env var.
      override = getEnv "IPFS_GATEWAY";
      gateway = if override == "" then "https://ipfs.io" else override;

      # Workaround for https://github.com/NixOS/nix/issues/12751
      # A derivation which copies 'src'. Since it's fixed-output, the resulting
      # 'outPath' is independent of 'src' (it only depends on 'narHash').
      fixed = src: derivation {
        name = "source";
        builder = "/bin/sh";
        system = currentSystem;
        outputHashMode = "nar";
        outputHash = narHash;
        args = [
          "-c"
          ''read -r -d "" content < ${src}; printf '%s\n' "$content" > "$out"''
        ];
      };
      # See if we already have an outPath for this narHash, by checking with a
      # dummy src: if so, use that path; otherwise call 'fetchTree'.
      existing = (fixed "/dev/null").outPath;
      file = if pathExists existing then existing else fixed (fetchTree {
        inherit narHash;
        type = "file";
        url = "${gateway}/ipfs/${cid}";
      });

      raw = import file;
    };
    # If we've been given a copy of nixpkgs, use that when fetching
    if nixpkgs == null
    then raw
    else (raw { pkgs = nixpkgs; }).fetchGitIPFS;

  fetchGitIPFS = nix-helpers.fetchGitIPFS or boot_fetchGitIPFS;
};
resolved
