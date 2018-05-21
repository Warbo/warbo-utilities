{ bash, nettools, procps, psmisc, wrap, xbindkeys, xcape, xdotool, xorg }:

wrap {
  name  = "keys";
  file  = ../raw/keys.sh;
  paths = [ bash nettools procps psmisc xbindkeys xcape xdotool xorg.setxkbmap
            xorg.xmodmap ];
}
