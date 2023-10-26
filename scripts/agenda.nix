{ bash, coreutils, gcalcli, gnugrep, gnused, raw, wrap }:

wrap {
  name = "agenda";
  file = raw."agenda.sh";
  paths = [ bash coreutils gcalcli gnugrep gnused ];
  vars = {
    LC_ALL = "C";
    LOCALE = "C";
  };
}
