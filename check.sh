#!/usr/bin/env bash
set -e

function fail {
    echo "$*" 1>&2
    exit 1
}

# Simple, quick sanity check. Useful as a git pre-commit hook.
find . -name "*.nix" -type f | while read -r F
do
    echo "Checking '$F'" 1>&2
    nix-instantiate --parse "$F" > /dev/null
done

echo "Looking for dodgy path references" 1>&2
find . -not -path '*/\.*' -name '*.nix' | while read -r F  # grep -R is slow
do
    if grep '\.\./raw' < "$F"
    then
        echo "Don't use 'raw' as a path in '$F', use the 'raw' variable" 1>&2
        echo "since that preserves relative paths between files."        1>&2
        exit 1
    fi
done

echo "Checking that haskell-nix derivations are cached" 1>&2
grep -R -l 'haskell-nix' | grep '\.nix$' | while read -r F
do
    grep 'plan-sha256' < "$F" > /dev/null || {
        echo "File '$F' uses haskell-nix without caching a plan-sha256" 1>&2
        fail "Build the package and follow the instructions in 'trace'"
    }
    grep 'materialized' < "$F" > /dev/null || {
        echo "File '$F' uses haskell-nix without a materialised plan"   1>&2
        fail "Build the package and follow the instructions in 'trace'"
    }

    GOT=$(grep -o 'raw\.[^;]*plan[^;]*' < "$F") ||
        fail "Couldn't find any '../raw' reference in haskell-nix file '$F'"
    D=$(echo "$GOT" | head -n1 | sed -e 's@raw\.@raw/@g' -e 's@$@/.plan.nix@g')
    unset GOT

    [[ -d "$D" ]] || fail "Couldn't find cache directory '$D' for '$F'"
    COUNT=$(find "$D" -type f -name '*.nix' | wc -l)
    [[ "$COUNT" -gt 0 ]] || fail "No .nix files in '$D'"
    if [[ "$COUNT" -eq 1 ]]
    then
        X=$(readlink -f "$D"/*.nix)
    else
        # There are multiple files which may contain the main definition we're
        # using. Try finding one whose name also appears in $F.
        FOUND=0
        for POSSIBLE in "$D"/*.nix
        do
            N=$(basename "$POSSIBLE" .nix)
            if grep "\"$N\"" < "$F" > /dev/null
            then
                [[ "$FOUND" -eq 0 ]] ||
                    fail "Ambiguity: Multiple files in '$D' are found in '$F'"
                X="$POSSIBLE"
            fi
        done
        unset FOUND
        unset POSSIBLE
    fi

       FOUNDNAME=$(grep 'identifier' < "$X"      |
                   grep -o 'name *= *"[^"]*"'    |
                   grep -o '"[^"]*"'             ) || fail "No name in '$X'"
    FOUNDVERSION=$(grep 'identifier' < "$X"      |
                   grep -o 'version *= *"[^"]*"' |
                   grep -o '"[^"]*"'             ) || fail "No version '$X'"
    unset D

    grep -F "$FOUNDNAME" < "$F" > /dev/null ||
        fail "Expected name '$FOUNDNAME' in '$F', not found"
    grep -F "$FOUNDVERSION" < "$F" > /dev/null ||
        fail "Expected version '$FOUNDVERSION' in '$F', not found"
    unset FOUNDNAME
    unset FOUNDVERSION
done

REPO="warbo-packages"
echo "Checking $REPO version" 1>&2

# Allow failure to get HEAD (e.g. in case we're offline)
if REV=$(git ls-remote "http://chriswarbo.net/git/$REPO.git" |
             grep HEAD | cut -d ' ' -f1 | cut -c1-7)
then
    grep "$REV" < helpers.nix || {
        echo "Didn't find $REPO rev '$REV' in helpers.nix" 1>&2
        exit 1
    }
    echo "Checking $REPO in helpers.nix builds (e.g. for SHA256)" 1>&2
    nix-build --no-out-link -A "$REPO" helpers.nix || exit 1
fi
