#!/usr/bin/env bash
set -e

mount | grep -q s3_repos || {
    echo "$HOME/Drives/s3_repos not mounted, skipping render" 1>&2
    exit 0
}

cd "$1"

NAME=$(basename "$PWD" .git)

PAGES=$(repoPath="$PWD" htmlInOut=1 inNixedDir genGitHtml)
export PAGES
DEST="$HOME/Drives/s3_repos/$NAME"

git2ipfs "$PWD" || echo "Failed to push to IPFS, carrying on anyway..." 1>&2

if [[ -n "$PAGES" ]]
then
    echo "Pushing '$PAGES' to Web" 1>&2

    #export RCLONE_S3_REGION=eu-west-1
    #export RCLONE_S3_PROVIDER=AWS
    #export RCLONE_S3_ENV_AUTH=true
    # Don't include repo.git, since we can use the real, canonical one
    #with-aws-creds rclone sync "$PAGES" ":s3:www.chriswarbo.net/git/$NAME"
    rsync --checksum --delete --ignore-times --progress --copy-unsafe-links \
          --archive --exclude /repo.git "$PAGES/" "$DEST"

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
