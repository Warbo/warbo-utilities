#!/usr/bin/env bash
set -e

mount | grep -q s3_repos || {
    echo "$HOME/Drives/s3_repos not mounted, skipping render" 1>&2
    exit 0
}

cd "$1"

NAME=$(basename "$PWD" .git)

# S3 will delete empty directories, but git checks for their presence. Create
# them if they don't exist.
if [[ -e HEAD ]]  # Only make dirs in something that looks like a git repo!
then
    for D in refs/heads refs/tags objects/info
    do
        [[ -e "$D" ]] || mkdir -p "$D"
    done
fi

PAGES=$(repoPath="$PWD" htmlInOut=1 inNixedDir genGitHtml)
export PAGES
DEST="$HOME/Drives/s3_repos/$NAME"

if [[ -n "$PAGES" ]]
then
    echo "Pushing '$PAGES' to Web" 1>&2

    #export RCLONE_S3_REGION=eu-west-1
    #export RCLONE_S3_PROVIDER=AWS
    #export RCLONE_S3_ENV_AUTH=true
    # Don't include repo.git, since we can use the real, canonical one
    #with-aws-creds rclone sync "$PAGES" ":s3:www.chriswarbo.net/git/$NAME"
    rsync --checksum --delete --ignore-times --progress --copy-unsafe-links \
          --archive --no-perms --no-owner --no-group \
          --exclude /repo.git --exclude /branches/master "$PAGES/" "$DEST" || {
        CODE="$?"
        if [[ "$CODE" -eq 23 ]]
        then
            echo "Ignoring rsync error about attributes (it's S3's fault)" 1>&2
        else
            echo "Unexpected rsync problem, aborting" 1>&2
            exit "$CODE"
        fi
    }

    # Edit repo link to point at canonical /git repo
    sed -e "s@repo.git@/git/$NAME.git@g" -i "$DEST/index.html"
fi

# Ensure there's no snapshot
F="$DEST/repo.git"
if [[ -e "$F" ]]
then
  echo "WARNING: Shouldn't have '$F', canonical URL should be used" 1>&2
fi

# Ensure we use canonical URL
URL="/git/$NAME.git"
  F="$DEST/index.html"

# shellcheck disable=SC2029
if grep -q "$URL" < "$F"
then
    true
else
    echo "WARNING: Didn't find canonical URL '$URL' in '$F'" 1>&2
fi
