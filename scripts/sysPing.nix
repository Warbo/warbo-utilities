{ bash, iputils, wrap }:

wrap {
  name   = "sysPing";
  paths  = [ bash iputils ];
  script = ''
    #!${bash}/bin/bash
    set -e
    [[ -e /run/wrappers/bin/ping ]] &&
    exec /run/wrappers/bin/ping "$@"

    [[ -e /var/setuid-wrappers/ping ]] &&
      exec /var/setuid-wrappers/ping "$@"

    exec ping "$@"
  '';
}
