{ bash, git, raw, wrap }:

wrap {
  name  = "trackGit";
  file  = raw."trackGit.sh";
  paths = [ bash git ];
}
