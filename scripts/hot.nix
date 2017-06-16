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
    if [[ -f ~/.tempThreshold ]]
    then
      TEMP_THRESHOLD=$(cat ~/.tempThreshold)
    else
      TEMP_THRESHOLD=85
    fi
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
