{ bash, openssh, wrap }:

wrap {
  name   = "jo";
  paths  = [ bash openssh ];
  script = ''
    #!/usr/bin/env bash
    export XAUTHORITY=/home/chris/.Xauthority

    CMD="export XAUTHORITY=/home/jo/.Xauthority; x2x -west -to :0"

    MACHINE=$(johost) || exit 1

    # shellcheck disable=SC2029
    ssh -Y jo@"$MACHINE" "$CMD"
  '';
}
