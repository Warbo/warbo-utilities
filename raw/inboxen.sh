#!/usr/bin/env bash
set -e
CODE=0

cd "$HOME"
echo "Fetching mail" 1>&2
for INBOX in gmail dundee
do
    if timeout -s 9 1800 mbsync --verbose "$INBOX"
    then
        echo "Finished syncing $INBOX" 1>&2
    else
        echo "Error syncing $INBOX" 1>&2
        CODE=1
    fi
done

# Try waiting for existing mu processes to die
for _ in $(seq 1 20)
do
    # Find running mu processes. Try to exclude mupdf, etc.
    # shellcheck disable=SC2009
    if P=$(ps auxww | grep '[ /]mu\( \|$\)')
    then
        echo "Stopping running mu instances" 1>&2
        echo "$P" | sed -e 's/  */ /g' | cut -d ' ' -f2 | while read -r I
        do
            kill -INT "$I"
        done
        sleep 1
    else
        # Stop early if nothing's running
        break
    fi
done

echo "Indexing maildirs for Mu" 1>&2
if mu index --maildir=~/Mail --lazy-check
then
    echo "Finished indexing" 1>&2
else
    echo "Error indexing" 1>&2
    CODE=2
fi

echo "Renaming up any leftover isync state files" 1>&2
echo "(These prevent syncing/indexing, but can be safely moved/deleted)" 1>&2
while read -r F
do
    mv -v "$F" "$F.deleteme"
done < <(find ~/Mail -name '*.journal')
exit "$CODE"
