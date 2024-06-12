# Like inNixedDir provided by nix-helpers, but this version doesn't make
# assumptions about what Nix env vars are supposed to be.
# TODO: The inNixedDir in nix-helpers should probably have those env vars
# removed, making it equivalent to this.
{
  bash,
  coreutils,
  nix-helpers,
  wrap,
}:
wrap {
  name = "inNixedDir";
  file = nix-helpers.inNixedDir.file;
  paths = [
    bash
    coreutils
  ];
  vars = { };
}
