{
  aws-login,
  coreutils,
  raw,
  writeShellApplication,
}:

"${
  writeShellApplication {
    name = "with-aws-creds";
    runtimeInputs = [
      aws-login
      coreutils
    ];
    text = builtins.readFile raw."with-aws-creds.sh";
  }
}/bin/with-aws-creds"
