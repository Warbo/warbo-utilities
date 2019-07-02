{ bash, get_iplayer, wrap }:

wrap {
  name   = "iget";
  paths  = [ bash get_iplayer ];
  script = ''
    #!/usr/bin/env bash
    get_iplayer --no-purge --modes=good --raw "$@"
  '';
}
