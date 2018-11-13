{ bash, pmutils, wrap }:

with {
  go = wrap {
    name   = "suspend_after.real";
    paths  = [ bash pmutils ];
    script = ''
      #!/usr/bin/env bash
      echo "Suspending after..." 1>&2
      countdown "$@" 1>&2
      pm-suspend
    '';
  };
};
wrap {
  name   = "suspend_after";
  paths  = [ bash ];
  script = ''
    #!/usr/bin/env bash
    SECS="$1"
    shift > /dev/null
    if [[ -z "$SECS" ]]
    then
      SECS=3600
      echo "No arguments given, defaulting to $SECS seconds" 1>&2
    fi
    sudo "${go}" "$SECS" "$@"
  '';
}
