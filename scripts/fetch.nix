{
  bash,
  raw,
  scripts,
  wget,
  wrap,
  xidel,
}:

wrap {
  name = "fetch";
  file = raw."fetch.sh";
  paths = [
    bash
    wget
    xidel
  ];
  vars = {
    inherit (scripts) fetch_podcasts youtube_then_mark;
  };
}
