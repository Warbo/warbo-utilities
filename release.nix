with rec {
  inherit (import ./. { packageOnly = false; }) pkg stablePkgs;
};
{
    stable = pkg;
  unstable = import ./. {
    # Unstable due to use of <nixpkgs> and latestGit
    nixPkgs = import <nixpkgs> { config = stablePkgs.latestNixCfg; };
  };
}
