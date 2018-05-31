{ bash, nettools, procps, psmisc, space2ctrl, wrap, xbindkeys, xdotool, xorg }:

wrap {
  name  = "keys";
  file  = ../raw/keys.sh;
  paths = [ bash nettools procps psmisc space2ctrl xbindkeys xdotool
            xorg.setxkbmap xorg.xmodmap ];
}
