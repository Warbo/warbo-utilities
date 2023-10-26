{ bash, inNixedDir, wrap, xmlstarlet }:

wrap {
  name = "pushgitpages";
  paths = [ inNixedDir xmlstarlet ];
  script = ''
    #!${bash}/bin/bash
    set -e

    cd "$1"

    NAME=$(basename "$PWD" .git)

    PAGES=$(repoPath="$PWD" htmlInOut=1 inNixedDir genGitHtml)
    export PAGES

    git2ipfs "$PWD" || echo "Failed to push to IPFS, carrying on anyway..." 1>&2

    D="/opt/html/$NAME"
    L="/opt/git/$NAME"

    if [[ -n "$PAGES" ]]
    then
        echo "Pushing '$PAGES' to Web" 1>&2

        # shellcheck disable=SC2029
        ssh chriswarbo.net "test -e '$D'" || {
          # shellcheck disable=SC2029
          ssh chriswarbo.net "sudo mkdir -p '$D'"
        }

        # shellcheck disable=SC2029
        ssh chriswarbo.net "test -e '$L'" || {
          # shellcheck disable=SC2029
          ssh chriswarbo.net "sudo ln -s '$D' '$L'"
        }

        # Don't include repo.git, since we can use the real, canonical one
        copyToWeb --exclude /repo.git "$PAGES/" "chriswarbo.net:$D"

        # Edit repo link to point at canonical /git repo
        # shellcheck disable=SC2029
        CONTENT=$(ssh chriswarbo.net "cat '$D/index.html'")

        # shellcheck disable=SC2029
        echo "$CONTENT" | sed -e "s@repo.git@/git/$NAME.git@g" |
          ssh chriswarbo.net "sudo tee '$D/index.html' > /dev/null"
    fi

    # Ensure we have a /git link to the /html pages
    # shellcheck disable=SC2029
    if ssh chriswarbo.net "test -h '$L'"
    then
        # shellcheck disable=SC2029
        FOUND=$(ssh chriswarbo.net "readlink '$L'")
        if [[ "$FOUND" = "$D" ]]
        then
            true
        else
            echo "WARNING: '$L' points to '$FOUND', not '$D'" 1>&2
        fi
    else
        echo "WARNING: '$L' is not a symlink to '$D'" 1>&2
    fi

    # Ensure there's no snapshot
    F="$D/repo.git"

    # shellcheck disable=SC2029
    if ssh chriswarbo.net "test -e '$F'"
    then
      echo "WARNING: Shouldn't have '$F', canonical URL should be used" 1>&2
    fi

    # Ensure we use canonical URL
    URL="/git/$NAME.git"
      F="$D/index.html"

    # shellcheck disable=SC2029
    if ssh chriswarbo.net "cat $F" |
       grep "$URL" > /dev/null
    then
        true
    else
        echo "WARNING: Didn't find canonical URL '$URL' in '$F'" 1>&2
    fi
  '';
}
