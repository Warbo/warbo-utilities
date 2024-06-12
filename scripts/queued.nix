{
  bash,
  raw,
  taskspooler,
  wrap,
}:

wrap {
  name = "queued";
  paths = [
    bash
    taskspooler
  ];
  file = raw."queued.sh";
}
