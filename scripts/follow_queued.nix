{ bash, raw, taskspooler, wrap }:

wrap {
  name = "follow_queued";
  paths = [ bash taskspooler ];
  file = raw."follow_queued.sh";
}
