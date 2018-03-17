{ bash, openssh, psmisc, synergy, writeScript }:

writeScript "josyn" ''
  #!${bash}/bin/bash

  MACHINE=$(johost) || exit 1

  if "${openssh}/bin/ssh" jo@"$MACHINE" true
  then
    echo "Killing synergy client for nixos" 1>&2
    "${openssh}/bin/ssh" jo@"$MACHINE" "./killsyn nixos"

    echo "Killing synergy server" 1>&2
    "${psmisc}/bin/killall" -9 synergys

    "${openssh}/bin/ssh" jo@"$MACHINE" "./synergy" &

    echo "Starting synergy server" 1>&2
    "${synergy}/bin/synergys"
  else
    echo "Can't connect to $MACHINE" 1>&2
    exit 1
  fi
''
