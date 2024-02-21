#!/usr/bin/env bash
set -eu
shopt -s nullglob

# Use Nix output directory, if requested

if [[ -n "${htmlInOut:-}" ]]
then
  [[ -z "${htmlPath:-}" ]] ||
    fail "'htmlInOut' and 'htmlPath' env vars are mutually exclusive"

  [[ -n "${out:-}" ]] ||
    fail "'htmlInOut' given, but found no 'out' in env"

  htmlPath="${out:-}"
fi

# If we've not been given env vars, take values from commandline args
if [[ -z "${repoPath:-}" ]] || [[ -z "${htmlPath:-}" ]]
then
  # Bail out if args weren't given
  [[ "$#" -eq 2 ]] || {
    echo "Usage: genGitHtml <repo-path> <destination>"               1>&2
    echo "Alternatively, set the 'repoPath' and 'htmlPath' env vars" 1>&2
    echo "Optionally, set REPO_LINK to your base URL"                1>&2
    exit 1
  }

  [[ -z "${repoPath:-}" ]] ||
    fail "env vars (like 'repoPath') and args are mutually exclusive"

  [[ -z "${htmlPath:-}" ]] ||
    fail "env vars (like 'htmlPath') and args are mutually exclusive"

  repoPath="$1"
  htmlPath="$2"
fi

if echo "$repoPath" | grep -q '^/'
then
  repoPath="file://$repoPath"
fi

repoName=$(basename "$repoPath" .git)

# Assume we're using chriswarbo.net, unless told otherwise
REPO_LINK="${REPO_LINK:-http://chriswarbo.net/git/$repoName.git}"

echo "Generating pages from git history" 1>&2
mkdir -p "$htmlPath/git"

#        Name           Source repo    Base URL        Destination
git2html -p "$repoName" -r "$repoPath" -l "$REPO_LINK" "$htmlPath/git" 1>&2

# We need to replace symlinks since S3 doesn't support them. This approach
# duplicates all the files; there might be a nicer way using redirects...
echo "Replacing symlinks in '$htmlPath/git'" 1>&2
while read -r SYMLINK
do
    TARGET=$(readlink -f "$SYMLINK")
    rm -v "$SYMLINK"
    cp -r "$TARGET" "$SYMLINK"
done < <(find "$htmlPath/git" -type l)

echo "Generating pages from issue tracker" 1>&2
mkdir "$htmlPath/issues"

if [[ -e "$htmlPath/git/repository/.issues" ]]
then
  mhonarc -mhpattern '^[^\.]' -outdir "$htmlPath/issues" \
          "$htmlPath"/git/repository/.issues/*/new \
          "$htmlPath"/git/repository/.issues/*/cur 1>&2
else
  # No issues, write a placeholder instead
  HTML='<html><body>No .issues</body></html>'
  echo "$HTML" > "$htmlPath/issues/threads.html"
  unset HTML
fi

echo "Generating index page" 1>&2

# Defaults
README_MSG="No README found"
READMEFILE="$htmlPath/readme.html"
echo '<span />' > "$READMEFILE"
export READMEFILE

echo "Looking for a README file" 1>&2
for F in README.md README README.txt
do
  if [[ -e "$htmlPath/git/repository/$F" ]]
  then
    README_MSG=$(echo -e "Contents of $F follows\\n\\n---\\n\\n")
    pandoc -f markdown -t html < "$htmlPath/git/repository/$F" \
                               > "$READMEFILE"
  fi
done

echo "Sanitising README HTML (if any), to prevent XSS" 1>&2
SANITISED="$htmlPath/readme.sanitised"
# shellcheck disable=SC2154
"$cleaner" < "$READMEFILE" > "$SANITISED"
rm "$READMEFILE"
mv "$SANITISED" "$READMEFILE"

echo "Getting latest commit date" 1>&2
pushd "$htmlPath/git/repository" 1>&2
  DATE=$(git log -n 1 --format=%ci)
popd 1>&2

function render() {
TICK='`'

cat <<EOF
*Last updated: $DATE*

Upstream URL: [${TICK}git clone $REPO_LINK${TICK}]($REPO_LINK)

[Repo](repo.git/)

[View repository](git/index.html)

[View issue tracker](issues/threads.html)

$README_MSG

READMESENTINEL

EOF
}

echo "Rendering..." 1>&2
render | pandoc \
             --standalone \
             -f markdown \
             --metadata title="$repoName" \
             -o "$htmlPath/index.pre.html"

echo "Splicing in README" 1>&2
# shellcheck disable=SC2154
"$splicer" < "$htmlPath/index.pre.html" > "$htmlPath/index.html"

rm "$htmlPath/index.pre.html"
rm "$READMEFILE"

# Kill the working tree used by git2html
rm -rf "$htmlPath/git/repository"

echo "Cloning a snapshot of the repo" 1>&2
git clone --bare "$repoPath" "$htmlPath/repo.git"
pushd "$htmlPath/repo.git" 1>&2
  # Unpack git data, so it dedupes better on IPFS
  git repack -A -d
  git update-server-info
  MATCHES=$(find objects/pack -maxdepth 1 -name '*.pack' -print -quit)
  if [[ -n "$MATCHES" ]]
  then
    cp objects/pack/*.pack .
    git unpack-objects < ./*.pack
    rm -f ./*.pack
  fi
popd 1>&2

echo "Removing mhonarc database" 1>&2
rm -f "$htmlPath/issues/.mhonarc.db"
