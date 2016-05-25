#!/usr/bin/env bash
shopt -s nullglob

BASE=$(dirname "$(readlink -f "$0")")

"$BASE/delete_crap.sh"
"$BASE/remove_empties.sh"
"$BASE/no_discs.sh"
"$BASE/free_out_of_commercial.sh"
"$BASE/no_dodgy_names.sh"
"$BASE/find_full_albums.sh"
"$BASE/normalise_whitespace.sh"
