{ bash, inNixedDir, raw, wrap, xmlstarlet }:

wrap {
  name = "pushgitpages";
  paths = [ inNixedDir xmlstarlet ];
  file = raw."pushGitPages.sh";
}
