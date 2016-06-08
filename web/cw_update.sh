#!/usr/bin/env bash

# Taken from http://unix.stackexchange.com/a/122624/63735
# 'sudo' will complain about no tty when run over SSH, so we call it from
# 'expect' to fake the presence of a tty. You can probably ignore the file
# descriptors, as they're just a "nice to have" for connecting our stdio to
# that of su_update.sh

expect -c "spawn -noecho sh -c {
     exec sudo ./su_update.sh >&4 2>&5 <&6 4>&- 5>&- 6<&-}
 exit [lindex [wait] 3]" 4>&1 5>&2 6<&0
