self: super:
with {
  drvs = self.callPackage ./derivation.nix {};
};
{
  warbo-utilities         = drvs.pkg;
  warbo-utilities-scripts = drvs.bin;
}
