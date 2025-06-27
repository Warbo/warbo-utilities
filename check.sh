#!/usr/bin/env bash
set -e

CODE=0
function fail {
    echo "$*" 1>&2
    CODE=1
    [[ -z "$FAILFAST" ]] || exit "$CODE"
}

# Simple, quick sanity check. Useful as a git pre-commit hook.
while read -r F
do
    echo "Checking '$F'" 1>&2
    nix-instantiate --parse "$F" > /dev/null || fail "Couldn't instantiate '$F'"
    if command -v nixfmt > /dev/null
    then
        nixfmt -w 80 -c "$F" || {
            fail "Unformatted '$F'"
            if [[ -n "$REFORMAT" ]]
            then
                nixfmt -w 80 "$F"
            else
                echo "Set REFORMAT to auto-format" 1>&2
            fi
        }
    fi
done < <(find . -name "*.nix" -type f)

echo "Looking for dodgy path references" 1>&2
while read -r F  # grep -R is slow
do
    if grep '\.\./raw' < "$F"
    then
        echo "Don't use 'raw' as a path in '$F', use the 'raw' variable" 1>&2
        fail "since that preserves relative paths between files."
    fi
done < <(find . -not -path '*/\.*' -name '*.nix')

echo "Checking dependencies are up to date" 1>&2
F="warbo-packages.nix"
diff "$F" <(update-nix-fetchgit < "$F") || {
    fail "Out of date: $F"
}

exit "$CODE"
