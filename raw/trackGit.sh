#!/usr/bin/env bash

REPOS="/home/chris/Programming/repos"

if ! ROOT=$(git rev-parse --show-toplevel)
then
    echo "You must be in a git repo to run trackGit"
    exit 1
fi

echo "Enter project name (.git will be appended automatically)"
read -r name

if [ ! -d "$REPOS/$name.git" ]
then
    (cd "$REPOS" || exit 1
     echo "Cloning $ROOT to $REPOS/$name.git"
     git clone --bare "$ROOT" "$name.git")
fi

echo "Pointing $name.git to chriswarbo.net"
(cd "$REPOS/$name.git" || exit 1
 git remote rm  origin
 git remote add origin "chris@chriswarbo.net:/opt/repos/$name.git"
 )

echo "Adding $REPOS/$name.git as a remote"
(cd "$ROOT" || exit 1
 git remote rm  origin
 git remote add origin "$REPOS/$name.git")

echo "Propagating changes"
(cd "$REPOS" || exit 1
 sh check.sh)
