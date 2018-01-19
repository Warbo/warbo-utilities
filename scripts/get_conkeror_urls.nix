{ bash, wrap, xsel }:

wrap {
  name   = "get_conkeror_urls";
  paths  = [ bash xsel ];
  script = ''
    #!/usr/bin/env bash
    conkeror -e 'writeToClipboard((function(s) { for_each_buffer(function(b) { s += b.document.location + "\n"; }); return s; })(""))'
    xsel --clipboard
  '';
}
