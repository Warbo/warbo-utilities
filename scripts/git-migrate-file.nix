{ bash, fail, git, wrap }:

wrap {
  name  = "git-migrate-file";
  file  = ../raw/git-migrate-file;
  paths = [ bash fail git ];
}
