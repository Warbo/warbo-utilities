{ bash, raw, wrap }:

wrap {
  name = "dual";
  paths = [ bash ];
  file = raw."dual.sh";
}
