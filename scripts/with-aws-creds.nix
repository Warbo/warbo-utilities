{
  coreutils,
  raw,
  writeShellApplication
}:

"${
  writeShellApplication {
    name = "with-aws-creds";
    runtimeInputs = [ # aws-login will be in PATH via the warbo-utilities wrapper
      coreutils
    ];
    text = builtins.readFile raw."with-aws-creds.sh";
  }
}/bin/with-aws-creds"
