#!/usr/bin/env bash
set -e

## Looks in the current directory for a git repo and integrates it with our git
## hosting infrastructure. This script should be idempotent, so we can run it in
## an existing repo to set up new parts of our infrastructure.

REPOS="/home/chris/Programming/repos"

# Ensure we're in an appropriate git repo, and get its top-level directory

if echo "$PWD" | grep "$REPOS" > /dev/null
then
    echo "trackGit should be called from a working copy, not a bare clone" 1>&2
    exit 1
fi

if ! ROOT=$(git rev-parse --show-toplevel)
then
    echo "trackGit must be run from a git repo. Try 'git init' first." 1>&2
    exit 1
fi

# Working dirs can be called anything, so check upstream for a repo name

origin=""
name=""
if git remote | grep '^origin$' > /dev/null
then
    # We have an 'origin' remote. Check if it points to our $REPOS dir
    origin=$(git remote get-url origin)
    if echo "$origin" | grep "$REPOS" > /dev/null
    then
        name=$(basename "$origin" .git)
    fi
fi

# Repo name is important since it's used in paths, etc. so confirm with user

if [[ -n "$name" ]]
then
    read -r -p "Found repo name '$name', is this correct? (Y/n)? " answer
    case "${answer:0:1}" in
        n|N )
            name=""
            ;;
    esac
fi

if [[ -z "$name" ]]
then
    echo "Enter project name (.git will be appended automatically)"
    read -r name
fi

# Make sure we have a local bare clone in $REPOS, following our naming scheme

localRepo="$REPOS/$name.git"

if [[ -d "$localRepo" ]]
then
    echo "Found existing '$localRepo'" 1>&2
else
    pushd "$REPOS" > /dev/null
        echo "Cloning $ROOT to $localRepo" 1>&2
        git clone --bare "$ROOT" "$name.git"
    popd > /dev/null
    pushd "$localRepo" > /dev/null
        git remote rm origin  # We're upstream, but default is downstream
    popd > /dev/null
fi

# Make sure the local bare clone is setup to push to a remote bare clone

remoteRepo="chris@chriswarbo.net:/opt/repos/$name.git"
pushd "$localRepo" > /dev/null
    rOrigin=""
    if git remote | grep '^origin$'
    then
        rOrigin=$(git remote get-url origin)
    fi
    if [[ "x$rOrigin" = "x$remoteRepo" ]]
    then
        echo "$localRepo is setup to push to chriswarbo.net" 1>&2
    else
        if [[ -n "$rOrigin" ]]
        then
            echo "Expected $localRepo origin $remoteRepo not '$rOrigin'" 1>&2
            read -r -p "Keep origin '$rOrigin' of $localRepo? (Y/n)? " answer
            case "${answer:0:1}" in
                n|N )
                    git remote rm origin
                    rOrigin=""
                    ;;
            esac
        fi
    fi
    if [[ -z "$rOrigin" ]]
    then
        echo "Pointing $localRepo to $remoteRepo" 1>&2
        git remote add origin "$remoteRepo"
    fi
popd > /dev/null

# Make sure the working dir is setup to push to the local bare clone

origin=""
if git remote | grep '^origin$' 1>&2
then
    # We have an 'origin' remote, we need to see if it's our $REPOS dir
    origin=$(git remote get-url origin)
fi
if [[ "x$origin" = "x$localRepo" ]]
then
    echo "$ROOT is setup to push to $localRepo" 1>&2
else
    if [[ -n "$origin" ]]
    then
        echo "Expected $ROOT origin $localRepo not '$origin'" 1>&2
        read -r -p "Keep origin '$origin' of $ROOT? (Y/n)? " answer
        case "${answer:0:1}" in
            n|N )
                git remote rm origin
                origin=""
                ;;
        esac
    fi
fi
if [[ -z "$origin" ]]
then
    echo "Adding $localRepo as remote origin for $ROOT" 1>&2
    git remote add origin "$localRepo"
fi

echo "Checking $REPOS in case we made changes which need propagating" 1>&2
cd "$REPOS" || exit 1
sh check.sh
