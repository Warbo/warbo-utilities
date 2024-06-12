{
  bash,
  raw,
  scripts,
  wrap,
}:

wrap {
  name = "youtube_then_mark";
  file = raw."youtube_then_mark.sh";
  paths = [ bash ];
  vars.yt = scripts.ytdl-wrapped;
}
