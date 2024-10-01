# Simple wrapper to run 'nixfmt -w 80' since Emacs nix-mode doesn't accept args.
{
  bash,
  nixfmt-rfc-style,
  raw,
  wrap,
}:

wrap {
  name = "nixfmt-80";
  paths = [
    bash
    nixfmt-rfc-style
  ];
  file = raw."nixfmt-80.sh";
}
