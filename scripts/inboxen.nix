{ bash, coreutils, gnused, iputils, isync, mu, procps, psutils, raw, scripts
, wrap }:

wrap {
  name = "inboxen";
  file = raw."inboxen.sh";
  paths = [ bash coreutils iputils isync mu procps psutils gnused ];
  vars = { };
}
