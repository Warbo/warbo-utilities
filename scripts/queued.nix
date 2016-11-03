#!/usr/bin/env bash

# Show output of running tasks

while true
do
  # Normal tasks
  ts -t

  echo "DONE NORMAL"

  # Tasks scheduled in a nix-shell end up in a different queue :(
  nix-shell -p ts --run "ts -t"

  echo "DONE SHELL"

  sleep 4
done
