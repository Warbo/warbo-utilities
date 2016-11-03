{ acpi, bc, writeScript }:

writeScript "hot" ''
  #!${bash}/bin/bash

  function temps {
    "${acpi}/bin/acpi" -V         |
      grep    "^Thermal"          |
      grep -v "trip point"        |
      grep -o "[0-9.]* degrees C" |
      grep -o "[0-9.]*"
  }

  THRESHOLD=80

  while read -r TEMP
  do
    COMP=$(echo "$TEMP > $THRESHOLD" | "${bc}/bin/bc")
    if [[ 1 -eq "$COMP" ]]
    then
      echo "$TEMP > $THRESHOLD" 1>&2
      exit 0
    fi
    echo "$TEMP < $THRESHOLD" 1>&2
  done < <(temps)

  exit 1
''
