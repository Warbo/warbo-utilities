{ bash, dtach, dvtm, wrap }:

wrap {
  name   = "shell";
  paths  = [ bash dtach ];
  vars   = {
    sesh = wrap {
      name   = "sesh";
      paths  = [ bash dvtm ];
      script = ''
        #!/usr/bin/env bash
        exec dvtm -M -m ^b
      '';
    };
  };
  script = ''
    #!/usr/bin/env bash
    exec dtach -A ~/.sesh -r winch "$sesh"
  '';
}
