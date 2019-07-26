{ bash, python2, raw, wrap }:

wrap {
  name   = "keepassxc-proxy-alternative";
  paths  = [ bash python2 ];
  script = ''
    #!${bash}/bin/bash
    set -e
    # Hard-code paths for all interpreters, so they'll run within chroots
    exec "${python2}/bin/python" "${raw.keepassxc-proxy-alternative}" "$@"
  '';
}
