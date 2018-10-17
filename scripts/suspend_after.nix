{ bash, pmutils, wrap }:

with {
  go = wrap {
    name   = "suspend_after.real";
    paths  = [ bash pmutils ];
    script = ''
      #!/usr/bin/env bash
      T="$1"
      while [[ "$T" -gt 1 ]]
      do
        T=$(( T - 1 ))
        echo "Suspend in $T..."
        sleep 1
      done
      pm-suspend
    '';
  };
};
wrap {
  name   = "suspend_after";
  paths  = [ bash ];
  script = ''
    #!/usr/bin/env bash
    if [[ -z "$T" ]]
    then
      T=3600
      echo "No T env var given, defaulting to $T seconds" 1>&2
    fi
    sudo "${go}" "$T"
  '';
}
