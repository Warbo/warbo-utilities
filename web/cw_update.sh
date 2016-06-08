#!/usr/bin/env bash
expect -c "spawn -noecho sh -c {
     exec sudo ./su_update.sh >&4 2>&5 <&6 4>&- 5>&- 6<&-}
 exit [lindex [wait] 3]" 4>&1 5>&2 6<&0
