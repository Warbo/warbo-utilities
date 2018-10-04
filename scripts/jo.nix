{ bash, openssh, wrap }:

wrap {
  name   = "jo";
  paths  = [ bash openssh ];
  script = ''
    #!/usr/bin/env bash
    export XAUTHORITY=/home/chris/.Xauthority

    [[ -n "$DISPLAY" ]] || DISPLAY=:0
    CMD="export XAUTHORITY=/home/jo/.Xauthority; x2x -west -to $DISPLAY"

    MACHINE=$(johost) || exit 1

    # shellcheck disable=SC2029
    ssh -Y jo@"$MACHINE" "$CMD"
  '';
}
