{ most, wrap }:

wrap {
  name   = "pager";
  paths  = [ most ];
  script = ''
    #!/usr/bin/env bash
    if [[ "x$TERM" = "xdumb" ]]
    then
      # We're probably in Emacs; let it handle the paging itself
      exec cat "$@"
    else
      exec most "$@"
    fi
  '';
}
