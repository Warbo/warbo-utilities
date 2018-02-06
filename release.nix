with rec {
  stable   = import ./. { packageOnly = false; };
  unstable = import ./. {
    packageOnly = false;
    nixPkgs     = import <nixpkgs> {
      config =
        with {
          config-src = with builtins.tryEval <nix-config>;
                       if success
                          then value
                          else stable.nixPkgs.latestGit {
                            url    = http://chriswarbo.net/git/nix-config.git;
                            stable = { unsafeSkip = true; };
                          };
        };
        import "${config-src}/config.nix";
    };
  };
};
{
    stable = builtins.removeAttrs   stable [ "nixPkgs" ];
  unstable = builtins.removeAttrs unstable [ "nixPkgs" ];
}
