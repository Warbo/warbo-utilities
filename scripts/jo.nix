#!/usr/bin/env bash
export XAUTHORITY=/home/chris/.Xauthority

CMD="export XAUTHORITY=/home/jo/.Xauthority; x2x -west -to :0"

if [[ -z "$JO_HOST" ]]
then
   JO_HOST=debian.local
fi

if ssh jo@"$JO_HOST" true 1> /dev/null 2> /dev/null
then
    echo "Found $JO_HOST, starting x2x"

    # shellcheck disable=SC2029
    ssh -Y jo@"$JO_HOST" "$CMD"
else
    echo "Couldn't connect to $JO_HOST" 1>&2
    exit 1
fi
