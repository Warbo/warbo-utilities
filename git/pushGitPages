#!/usr/bin/env bash
set -e

cd "$1"

NAME=$(basename "$PWD" .git)

# Duplicate stderr so we can a) display it and b) pick out the Nix store dir
exec 5>&1
SAVED=$(git2ipfs "$PWD" 2>&1 | tee >(cat - >&5)       |
                               grep "Saved in "       |
                               sed -e 's/Saved in //g')

if [[ -n "$SAVED" ]]
then
    echo "Pushing '$SAVED' to Web" 1>&2

    # Don't include repo.git, since we can use the real, canonical one
    copyToWeb --exclude /repo.git "$SAVED/" "chriswarbo.net:/opt/html/$NAME"

    # Edit "snapshot" link to point at canonical /git repo
    CONTENT=$(ssh chriswarbo.net "cat /opt/$NAME/index.html")
       LINK="<a href='/git/$NAME.git'>Canonical repo</a>"

    echo "$CONTENT" | xmlstarlet ed -r '//a[@href="repo.git/"]' -v "$LINK" - |
                      ssh chriswarbo.net "cat > /opt/html/$NAME/index.html"
fi

# Ensure we have a /git link to the /html pages
SRC="$/opt/html/$NAME"
DST="/opt/git/$NAME"
if ssh chriswarbo.net "test -h '$DST'"
then
    FOUND=$(ssh chriswarbo.net "readlink '$DST'")
    if [[ "x$FOUND" = "x$SRC" ]]
    then
        true
    else
        echo "WARNING: '$DST' points to '$FOUND', not '$SRC'" 1>&2
    fi
else
    echo "WARNING: '$DST' is not a symlink to '$SRC'" 1>&2
fi

# Ensure our repo.git redirects to /git/foo.git
if ssh chriswarbo.net "cat /opt/html/$NAME/repo.git" |
        grep "/opt/git/$NAME.git" > /dev/null
then
    true
else
    echo /opt/html/$NAME/repo.git"
