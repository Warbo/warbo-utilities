#!/usr/bin/env bash
set -e

url="$1" # Get the first argument

if [[ -z "$url" ]]; then
    echo "Usage: $0 <github_url>"
    echo "Supported formats:"
    echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch"
    echo "  https://github.com/owner/repo/commit/hash"
    echo "  https://github.com/owner/repo/tree/branch"
    echo "  https://github.com/owner/repo (assumes 'main' branch)"
    exit 1
fi

api_url=""

if [[ "$url" =~ ^https://api\.github\.com/repos/ ]]; then
    # Already an API URL
    api_url="$url"
elif [[ "$url" =~ ^https://github\.com/ ]]; then
    # Standard GitHub URL
    # Extract owner/repo
    owner_repo=$(echo "$url" | sed -E 's|https://github\.com/([^/]+/[^/]+).*|\1|')

    if [[ "$url" =~ /commit/ ]]; then
        # URL with commit hash
        commit_hash=$(echo "$url" | sed -E 's|.*/commit/([^/]+).*|\1|')
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_hash}"
    elif [[ "$url" =~ /tree/ ]]; then
        # URL with branch/tag
        branch_tag=$(echo "$url" | sed -E 's|.*/tree/([^/]+).*|\1|')
        api_url="https://api.github.com/repos/${owner_repo}/commits/${branch_tag}"
    else
        # URL without commit or tree, assume default branch (main)
        api_url="https://api.github.com/repos/${owner_repo}/commits/main"
        # Note: This assumes 'main'. A more robust script might check for 'master' or fetch the default branch.
    fi
else
    echo "Error: Unrecognized URL format: $url"
    echo "Supported formats:"
    echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch"
    echo "  https://github.com/owner/repo/commit/hash"
    echo "  https://github.com/owner/repo/tree/branch"
    echo "  https://github.com/owner/repo (assumes 'main' branch)"
    exit 1
fi

# Now use the constructed api_url to fetch the commit and extract the tree sha
curl -s "$api_url" | jq -r '.commit.tree.sha'
