{ bash, coreutils, raw, wrap }:
wrap {
  name = "inNixedDir";
  file = raw."inNixedDir.sh";
  paths = [ bash coreutils ];
  vars = { };
}
