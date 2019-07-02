{ wrap }:

wrap {
  name   = "iget";
  script = ''
    #!/usr/bin/env bash
    get_iplayer --no-purge --modes=good --raw "$@"
  '';
}
