{
  coreutils,
  scripts, # Add 'scripts' to the arguments
  raw,
  writeShellApplication,
}:

"${
  writeShellApplication {
    name = "with-aws-creds";
    runtimeInputs = [
      scripts.aws-login # Access aws-login from the 'scripts' attribute
      coreutils
    ];
    text = builtins.readFile raw."with-aws-creds.sh";
  }
}/bin/with-aws-creds"
