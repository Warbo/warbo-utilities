{ acpi, bash, bc, writeScript }:

writeScript "hot" ''
  #!${bash}/bin/bash

  function temps {
    "${acpi}/bin/acpi" -V         |
      grep    "^Thermal"          |
      grep -v "trip point"        |
      grep -o "[0-9.]* degrees C" |
      grep -o "[0-9.]*"
  }

  if [[ -z "$TEMP_THRESHOLD" ]]
  then
    TEMP_THRESHOLD=90
  fi

  while read -r TEMP
  do
    COMP=$(echo "$TEMP > $TEMP_THRESHOLD" | "${bc}/bin/bc")
    if [[ 1 -eq "$COMP" ]]
    then
      echo "$TEMP > $TEMP_THRESHOLD" 1>&2
      exit 0
    fi
    echo "$TEMP < $TEMP_THRESHOLD" 1>&2
  done < <(temps)

  exit 1
''
