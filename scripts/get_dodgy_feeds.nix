{ bash, raw, wget, wrap, xidel }:

wrap {
  name  = "get_dodgy_feeds";
  file  = raw."get_dodgy_feeds.sh";
  paths = [ bash wget xidel ];
}
