{ bash, get_iplayer, wrap }:

wrap {
  name = "iget";
  paths = [ bash get_iplayer ];
  script = ''
    #!${bash}/bin/bash
    get_iplayer --no-purge --modes=good --raw "$@"
  '';
}
