{ bash, lynx, raw, wrap, writeScript, xidel }:

with rec {
  vse = wrap {
    name  = "getvid-vse";
    paths = [ bash lynx ];
    vars  = {
      COLUMNS = "1000";
      cmd     = writeScript "vse-keys" ''
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
    script = ''
      #!/usr/bin/env bash
      lynx -term=linux -accept_all_cookies -cmd_script="$cmd" "$1" |
        grep 'video/mp4' | grep -o 'http[^"]*'
    '';
  };
};
wrap {
  name  = "getvid";
  paths = [ bash xidel ];
  vars  = {
    inherit vse;
    list = raw."listepurls.sh";
  };
  script = ''
    #!/usr/bin/env bash
    set -e

    function esc {
      # shellcheck disable=SC1003
      sed -e "s/'/'"'\\'"'''/g"
    }

    # shellcheck disable=SC2154
    LINKS=$("$list" "$@")

    echo "LINKS: $LINKS" 1>&2

    echo "$LINKS" | grep '/vs...e\.e' | while read -r PAIR
    do
      LINK=$(echo "$PAIR" | cut -f2)
      [[ -n "$LINK" ]] || continue

      # shellcheck disable=SC2154
      URL=$("$vse" "$LINK") || true
      [[ -n "$URL" ]] || continue
      URL=$(echo "$URL" | esc)

      TITLE=$(echo "$PAIR" | cut -f1 | esc)
      [[ -n "$TITLE" ]] || TITLE="UNKNOWN"

      echo "wget -O '$TITLE' '$URL'"
    done
  '';
}
