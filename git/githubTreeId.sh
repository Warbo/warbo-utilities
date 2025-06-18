#!/usr/bin/env bash
set -e

url="$1" # Get the first argument

if [[ -z "$url" ]]; then
    echo "Usage: $0 <github_url>" >&2
    echo "Supported formats:" >&2
    echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch" >&2
    echo "  https://github.com/owner/repo/commit/hash" >&2
    echo "  https://github.com/owner/repo/tree/branch" >&2
    echo "  https://github.com/owner/repo (fetches default branch)" >&2
    exit 1
fi

api_url=""
owner_repo=""
commit_ref="" # Can be hash or branch name

if [[ "$url" =~ ^https://api\.github\.com/repos/([^/]+/[^/]+)/commits/([^/]+) ]]; then
    # Already an API URL for commits
    owner_repo=${BASH_REMATCH[1]}
    commit_ref=${BASH_REMATCH[2]}
    api_url="$url"
elif [[ "$url" =~ ^https://github\.com/([^/]+/[^/]+) ]]; then
    # Standard GitHub URL
    owner_repo=${BASH_REMATCH[1]}

    if [[ "$url" =~ /commit/([^/]+) ]]; then
        # URL with commit hash
        commit_ref=${BASH_REMATCH[1]}
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
    elif [[ "$url" =~ /tree/([^/]+) ]]; then
        # URL with branch/tag
        commit_ref=${BASH_REMATCH[1]}
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
    elif [[ "$url" =~ ^https://github\.com/([^/]+/[^/]+)$ ]]; then
        # URL is just https://github.com/owner/repo
        # Fetch default branch
        repo_api_url="https://api.github.com/repos/${owner_repo}"
        default_branch=$(curl -s "$repo_api_url" | jq -r '.default_branch')
        if [[ "$default_branch" == "null" || -z "$default_branch" ]]; then
            echo "Error: Could not determine default branch for $owner_repo" >&2
            exit 1
        fi
        commit_ref="$default_branch"
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
    else
        echo "Error: Unrecognized GitHub URL format: $url" >&2
        echo "Supported formats:" >&2
        echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch" >&2
        echo "  https://github.com/owner/repo/commit/hash" >&2
        echo "  https://github.com/owner/repo/tree/branch" >&2
        echo "  https://github.com/owner/repo (fetches default branch)" >&2
        exit 1
    fi
else
    echo "Error: Unrecognized URL format: $url" >&2
    echo "Supported formats:" >&2
    echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch" >&2
    echo "  https://github.com/owner/repo/commit/hash" >&2
    echo "  https://github.com/owner/repo/tree/branch" >&2
    echo "  https://github.com/owner/repo (fetches default branch)" >&2
    exit 1
fi

# Now use the constructed api_url to fetch the commit and extract the tree sha
curl -s "$api_url" | jq -r '.commit.tree.sha'
