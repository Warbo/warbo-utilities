{
  awscli,
  coreutils,
  gnused,
  jq,
  pass,
  raw,
  writeShellApplication,
  writeShellScript,
}:
"${
  writeShellApplication {
    name = "aws-login";
    text = builtins.readFile raw."aws-login.sh";
    runtimeInputs = [
      coreutils
      gnused
      jq
      pass
      awscli
    ];
  }
}/bin/aws-login"
