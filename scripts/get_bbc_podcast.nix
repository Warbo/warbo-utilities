{
  bash,
  raw,
  wget,
  wrap,
  xidel,
}:

wrap {
  name = "get_bbc_podcast";
  file = raw."get_bbc_podcast.sh";
  paths = [
    bash
    wget
    xidel
  ];
}
