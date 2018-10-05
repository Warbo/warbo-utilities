{ bash, openssh, wrap }:

wrap {
  name   = "jo";
  paths  = [ bash openssh ];
  script = ''
    #!/usr/bin/env bash
    export XAUTHORITY=/home/chris/.Xauthority

    [[ -n "$REMOTE_DISPLAY" ]] || REMOTE_DISPLAY=:1
    CMD="export XAUTHORITY=/home/jo/.Xauthority; x2x -west -to $REMOTE_DISPLAY"

    MACHINE=$(johost) || exit 1

    # shellcheck disable=SC2029
    ssh jo@"$MACHINE" "$CMD"
  '';
}
