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

# Check if we need to generate HTML pages for this repo
HTML="/opt/html/$name"
if ssh chriswarbo.net true
then
    # shellcheck disable=SC2029
    if ssh chriswarbo.net "test -e '$HTML/index.html'"
    then
        echo "Found existing HTML directory '$HTML' on chriswarbo.net" 1>&2
    else
        MAKEHTML=1
        echo "No HTML '$HTML/index.html' found on chriswarbo.net" 1>&2
        read -r -p "Should we create 'chriswarbo.net:$HTML'? (Y/n)? " answer
        case "${answer:0:1}" in
            n|N )
                MAKEHTML=0
                ;;
        esac
        [[ "$MAKEHTML" -eq 0 ]] || pushGitPages "$localRepo"
        unset MAKEHTML
    fi
else
    echo "Couldn't ssh to chriswarbo.net, not checking for HTML pages" 1>&2
fi

# Check if we have an asv.conf.json
ASV_CONF=0
# shellcheck disable=SC2034
while read -r F
do
    ASV_CONF=1
done < <(find . -name 'asv.conf.json')

# If we have no asv.conf.json, ask to make one
ASV_TEMPLATE="$HOME/.templates/asv.conf.json.template"
if [[ "$ASV_CONF" -eq 0 ]] && [[ -e "$ASV_TEMPLATE" ]]
then
    read -r -p "No asv.conf.json, create from $ASV_TEMPLATE? (y/N)? " answer
    case "${answer:0:1}" in
        y|Y )
            ASV_CONF=1
            ;;
    esac
    if [[ "$ASV_CONF" -eq 1 ]]
    then
        sed -e "s/PROJECT_NAME/$name/g" < "$ASV_TEMPLATE" > asv.conf.json
    fi
fi
unset ASV_TEMPLATE

# If we have an asv.conf.json in the default location, see if its benchmark_dir
# already exists, and ask to create if not
ASV_CONF="$PWD/asv.conf.json"
if [[ -e "$ASV_CONF" ]]
then
    BENCHMARK_DIR=$(grep -v '^ *//' < "$ASV_CONF" | jq -r '.benchmark_dir')
    BENCHMARK_DIR="$PWD/$BENCHMARK_DIR"
    [[ -e "$BENCHMARK_DIR" ]] || {
        read -r -p "Dir $BENCHMARK_DIR not found, create (y/N)? " answer
        case "${answer:0:1}" in
            y|Y )
                mkdir -p "$BENCHMARK_DIR"
                ;;
        esac
    }

    # Benchmark dir should contain an __init__.py to avoid problems
    [[ -e "$BENCHMARK_DIR" ]] && {
        BENCHMARK_PY="$BENCHMARK_DIR/__init__.py"
        [[ -e "$BENCHMARK_PY" ]] || {
            read -r -p "No $BENCHMARK_PY found, create empty one (y/N)? " answer
            case "${answer:0:1}" in
                y|Y )
                    touch "$BENCHMARK_PY"
                    ;;
            esac
        }
        unset BENCHMARK_PY
    }

    # Should we ask to create a default.nix in the benchmark_dir?
    BENCHMARK_TEMPLATE="$HOME/.templates/benchmark_default.template"
    if [[ -e "$BENCHMARK_DIR" ]] && [[ -e "$BENCHMARK_TEMPLATE" ]]
    then
        BENCHMARK_NIX="$BENCHMARK_DIR/default.nix"
        [[ -e "$BENCHMARK_NIX" ]] || {
            read -r -p "No $BENCHMARK_NIX, create one? (y/N)? " answer
            case "${answer:0:1}" in
                y|Y )
                    cp -v "$BENCHMARK_TEMPLATE" "$BENCHMARK_NIX"
                    ;;
            esac
        }
        unset BENCHMARK_NIX
    fi
    unset BENCHMARK_TEMPLATE
    unset BENCHMARK_DIR
fi
unset ASV_CONF

echo "Checking $REPOS in case we made changes which need propagating" 1>&2
cd "$REPOS" || exit 1
sh check.sh
