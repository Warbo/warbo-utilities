#!/usr/bin/env bash
set -e

[[ "$#" -gt 0 ]] || {
    echo "Need repo directories as arguments" 1>&2
    exit 1
}

REPOS="$HOME/Drives/s3_repos"
if mount | grep -q "$(basename "$REPOS")"
then
    true
else
    export RCLONE_S3_REGION=eu-west-1
    export RCLONE_S3_PROVIDER=AWS
    export RCLONE_S3_ENV_AUTH=true
    with-aws-creds rclone mount \
                   --vfs-cache-mode=full \
                   --no-update-modtime --checksum \
                   ":s3:www.chriswarbo.net/git" \
                   "$REPOS" &
fi

COUNT=10
while [[ "$COUNT" -gt 0 ]]
do
    [[ -e "$REPOS/panpipe" ]] && break
    COUNT=$(( COUNT - 1 ))
    printf '.' 1>&2
    sleep 2
done
if [[ -e "$REPOS/panpipe" ]]
then
    printf '\nS3 mounted on %s\n' "$REPOS" 1>&2
else
    printf '\nDid not find repos in %s, aborting\n' "$REPOS" 1>&2
    exit 1
fi

function check() {
    if git remote | grep -q 'origin'
    then
        echo "Remote '$PWD' shouldn't have origin remote; removing" 1>&2
        git remote rm origin
    else
        true
    fi
    if git remote | grep -q 'github'
    then
        true
    else
        if GITHUB_REMOTE=$(cd "$1" && git remote | grep -i 'github') &&
           [[ -n "$GITHUB_REMOTE" ]]
        then
            GITHUB_URL=$(cd "$1" && git remote show -n "$GITHUB_REMOTE" |
                                 grep -io '[^ ]*github.com[^ ]*' | head -n1)
            if [[ -n "$GITHUB_URL" ]]
            then
                echo "Adding '$GITHUB_URL' as remote 'github' to '$PWD'" 1>&2
                git remote add github "$GITHUB_URL"
            fi
        fi
    fi
    if [[ -e hooks/post-receive ]]
    then
        true
    else
        F="$HOME/repos/warbo-utilities/git/post-receive.hook.local"
        if [[ -e "$F" ]]
        then
            echo "Copying post-receive hook '$F' to '$PWD'" 1>&2
            cp -v "$F" hooks/post-receive
        fi
    fi
}

for DIR in "$@"
do
    NAME=$(basename "$DIR" .git)
    SRC=$(readlink -f "$DIR")
    DEST="$REPOS/$NAME.git"
    if [[ -e "$DEST" ]]
    then
        echo "Checking '$DEST'" 1>&2
        (
            cd "$DEST"
            check "$SRC"
        )
        echo "Pushing '$SRC' to '$DEST'" 1>&2
        (cd "$SRC"; git push --all "$DEST")
    else
        echo "Cloning '$SRC' to '$DEST'" 1>&2
        git clone --bare "$SRC" "$DEST"
        (
            cd "$DEST"
            check "$SRC"
            [[ -e 'hooks/post-receive' ]] && bash hooks/post-receive
        )
    fi
    (
        cd "$SRC"
        if git remote | grep -q origin &&
                git remote show -n origin | grep -q 'chriswarbo.net/git'
        then
            git remote rename origin chriswarbo-net
            git remote add origin "$DEST"
            git fetch --all
            if git branch -a | grep -q origin/master
            then
                git branch -u origin/master master
                git fetch --all
            fi
        fi
    )
done
echo
