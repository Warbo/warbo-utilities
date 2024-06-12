{
  bash,
  fail,
  raw,
  wrap,
  xdotool,
}:

wrap {
  name = "emacsen";
  file = raw.emacsen;
  paths = [
    bash
    fail
    xdotool
  ];
  vars = {
    DISPLAY = ":0";
  };
}
