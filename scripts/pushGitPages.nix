{
  bash,
  raw,
  wrap,
  xmlstarlet,
}:

wrap {
  name = "pushgitpages";
  paths = [ xmlstarlet ];
  file = raw."pushGitPages.sh";
}
