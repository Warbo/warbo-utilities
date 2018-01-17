with builtins;
with rec {
  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  stableConfig = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "d1b2b9b";
    sha256 = "1rsax2izq5083wlxssg0ch4bxkg2g1hm2v61vp8frg5v9q55rlgr";
  };

  # An awkward mix of unstable <nixpkgs> and stable nix-config. We only use this
  # to fetch other, purely stable/unstable package sets
  bootstrapPkgs = import <nixpkgs> {
    config = import "${stableConfig}/stable.nix";
  };

  # Uses stable config with stable nixpkgs (repo1609 is fixed-output)
  stablePkgs = import bootstrapPkgs.repo1609 {
    config = import "${stableConfig}/stable.nix";
  };
};

{
  # The nixpkgs set to use, e.g. if we want a particular revision. Should use
  # some version of nix-config, else our dependencies will be missing.
  nixPkgs ? stablePkgs,

  # If false, returns some of our intermediate results alongside the package
  packageOnly ? true
}:

with nixPkgs.lib;
with rec {
  scripts = mapAttrs' (f: _: {
                        name  = removeSuffix ".nix" f;
                        value = nixPkgs.callPackage (./scripts + "/${f}") {};
                      })
                      (readDir ./scripts);

  mkCmd = name: script: ''
    makeWrapper "${script}" "$out/bin/${name}" --prefix PATH : "$out/bin"
  '';

  cmds = attrValues (mapAttrs mkCmd scripts);

  pkg = with nixPkgs; stdenv.mkDerivation {
    name                  = "warbo-utilities";
    src                   = ./.;
    buildInputs           = [ makeWrapper ];
    propagatedBuildInputs = [ python ];
    installPhase          = ''
      mkdir -p "$out/bin"
      for DIR in svn system web git development testing docs
      do
        cp "$DIR/"* "$out/bin/"
      done
      ${concatStringsSep "\n" cmds}
    '';
  };
};

if packageOnly
   then pkg
   else { inherit cmds scripts pkg stablePkgs; }
