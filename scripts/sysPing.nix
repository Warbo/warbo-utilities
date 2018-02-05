{ bash, iputils, wrap }:

wrap {
  name   = "sysPing";
  paths  = [ bash iputils ];
  script = ''
    #!/usr/bin/env bash
    set -e
    [[ -e /run/wrappers/bin/ping ]] &&
    exec /run/wrappers/bin/ping "$@"

    [[ -e /var/setuid-wrappers/ping ]] &&
      exec /var/setuid-wrappers/ping "$@"

    exec ping "$@"
  '';
}
