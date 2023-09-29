{ bash, raw, wget, wrap, xidel }:

wrap {
  name = "getmeetup";
  file = raw."getmeetup.sh";
  paths = [ bash wget xidel ];
}
