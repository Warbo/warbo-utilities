{ bash, git, raw, wrap }:

wrap {
  name  = "artemis";
  file  = raw.artemis;
  paths = [ bash git ];
}
