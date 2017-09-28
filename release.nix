with rec {
  # stablePkgs uses nix-config, which gives us latestGit
  inherit (import ./. { packageOnly = false; }) pkg stablePkgs withConfig;
};
{
    stable = pkg;  # Use as-is; defaults to stable nixpkgs and nix-config
  unstable = import ./. {
    # Unstable due to use of <nixpkgs> and latestGit
    nixPkgs = withConfig <nixpkgs> (stablePkgs.latestGit {
      url = http://chriswarbo.net/git/nix-config.git;
    });
  };
}
