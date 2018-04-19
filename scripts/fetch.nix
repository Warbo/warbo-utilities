{ bash, fetch_podcasts, wget, wrap, xidel, youtube_then_mark, yt }:

wrap {
  name  = "fetch";
  file  = ../raw/fetch.sh;
  paths = [ bash wget xidel ];
  vars  = { inherit fetch_podcasts youtube_then_mark yt; };
}
