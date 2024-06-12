{
  bash,
  feh,
  wrap,
}:

wrap {
  name = "setBg";
  paths = [
    bash
    feh
  ];
  script = ''
    #!${bash}/bin/bash
    PIC="$HOME/Pictures/Backgrounds/01141_hal9000_1280x1024.jpg"
    if [[ -e "$PIC" ]]
    then
      feh --bg-fill "$PIC"
    fi
  '';
}
