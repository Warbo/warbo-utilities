{ bash, fail, jq, jsbeautifier, lynx, raw, wget, wrap, writeScript, xidel
, youtube-dl }:

with rec {
  f5 = wrap {
    name = "getvid-f5";
    paths = [ bash jsbeautifier xidel ];
    file = raw."getvid-f5.sh";
  };

  voza = wrap {
    name = "getvid-voza";
    paths = [ bash wget xidel ];
    file = raw."getvid-voza.sh";
  };

  vse = wrap {
    name = "getvid-vse";
    file = raw."getvid-vse.sh";
    paths = [ bash lynx ];
    vars = {
      COLUMNS = "1000";
      cmd = writeScript "vse-keys" ''
        # Command logfile created by Lynx 2.8.9dev.16 (11 Jul 2017)

        # Submit form
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key Down Arrow
        key ^J

        # View source
        key \
        key y

        # Print to screen
        key p
        key Down Arrow
        key Down Arrow
        key ^J

        # Confirm
        key y
        key ^J

        # Exit
        key ^J
        key q
        key y
      '';
    };
  };
};
wrap {
  name = "getvid";
  file = raw."getvid.sh";
  paths = [ bash xidel ];
  vars = {
    inherit f5 voza vse;
    list = raw."listepurls.sh";
    msg = ''
      Usage: getvid <listing url>

      Looks through a listing of providers, printing 'TITLE\tURL' to stderr for
      each. Loops through each to see if (a) it has a handler (youtube-dl or
      custom) and (b) whether the handler returns a working URL. If so, a
      command for fetching from that provider is written to stdout.
      Set DEBUG=1 to see each handler running.

      Known handlers (e.g. for running standalone) are:
        ${f5}
        ${voza}
        ${vse}
    '';
  };
}
