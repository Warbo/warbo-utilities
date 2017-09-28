with builtins;
with rec {
  # We need a few helpers and packages from nix-config, so default to a
  # known-good version
  config = (import <nixpkgs> {}).fetchgit {
    url    = http://chriswarbo.net/git/nix-config.git;
    rev    = "00ef7f96541ed24ef72c69088620f27e6a3c8f5f";
    sha256 = "1b7g4r144hwqa2a13cnfwmwxfkjd884pk4lqralxiqwbb0vr0nsw";
  };

  withConfig = nixpkgs: config: import nixpkgs {
    config = import "${config}/config.nix";
  };

  # <nixpkgs> is unstable since it could be any version, but repo1609 is a
  # fixed-output derivation, which removes the instability.
  stablePkgs = withConfig (withConfig <nixpkgs> config).repo1609 config;
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
   else { inherit pkg stablePkgs withConfig; }
