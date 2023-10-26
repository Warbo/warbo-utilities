#!/usr/bin/env bash
set -e
wget -q -O- "$1"                                                  |
  xidel -s - -e '//script[contains(text(),"p,a,c,k,e,d")]/text()' |
  js-beautify -                                                   |
  grep -v '\.srt"'                                                |
  grep -o 'file: *"[^"]*'                                         |
  grep -o '".*'                                                   |
  tr -d '"'                                                       |
  head -n1
