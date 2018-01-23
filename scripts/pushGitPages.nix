{ wrap, xmlstarlet }:

wrap {
  paths  = [ xmlstarlet ];
  script = ''
    #!/usr/bin/env bash
    set -e

    cd "$1"

    NAME=$(basename "$PWD" .git)

    # Duplicate stderr so we can a) display it and b) pick out the Nix store dir
    exec 5>&1
    SAVED=$(git2ipfs "$PWD" 2>&1 | tee >(cat - >&5)       |
                                   grep "Saved in "       |
                                   sed -e 's/Saved in //g')

    D="/opt/html/$NAME"
    L="/opt/git/$NAME"

    if [[ -n "$SAVED" ]]
    then
        echo "Pushing '$SAVED' to Web" 1>&2

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
        copyToWeb --exclude /repo.git "$SAVED/" "chriswarbo.net:$D"

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
        if [[ "x$FOUND" = "x$D" ]]
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
