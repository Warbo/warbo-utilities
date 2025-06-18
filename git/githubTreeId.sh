#!/usr/bin/env bash
set -e

usage() {
    echo "Usage: $0 <github_url>" >&2
    echo "Supported formats:" >&2
    echo "  https://api.github.com/repos/owner/repo/commits/hash_or_branch" >&2
    echo "  https://github.com/owner/repo/commit/hash" >&2
    echo "  https://github.com/owner/repo/tree/branch" >&2
    echo "  https://github.com/owner/repo (fetches default branch)" >&2
}

url="$1" # Get the first argument

if [[ -z "$url" ]]; then
    usage
    exit 1
fi

api_url=""
owner_repo=""
commit_ref="" # Can be hash or branch name
is_branch=false # Flag to indicate if commit_ref is a branch name

if [[ "$url" =~ ^https://api\.github\.com/repos/([^/]+/[^/]+)/commits/([^/]+) ]]; then
    # Already an API URL for commits
    owner_repo=${BASH_REMATCH[1]}
    commit_ref=${BASH_REMATCH[2]}
    # Cannot reliably determine if it's a branch or hash from API URL alone without another call
    # We'll assume it's a hash unless it matches a known branch pattern later if needed, but for now, just use the ref.
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
        is_branch=true
    elif [[ "$url" =~ ^https://github\.com/([^/]+/[^/]+)$ ]]; then
        # URL is just https://github.com/owner/repo
        # Fetch default branch
        repo_api_url="https://api.github.com/repos/${owner_repo}"
        repo_info=$(curl -s "$repo_api_url")
        # Use jq -re to extract default_branch and exit non-zero if null/empty
        if default_branch=$(echo "$repo_info" | jq -re '.default_branch'); then
            # Success: default_branch was set and jq exited 0
            commit_ref="$default_branch"
            api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
            is_branch=true
        else
            # Failure: jq exited non-zero (null or empty)
            echo "Error: Could not determine default branch for $owner_repo (jq extraction failed)" >&2
            # Optionally check if repo_info was empty or an error message from curl/github
            if [[ -z "$repo_info" ]]; then
                 echo "Error: Received empty response from $repo_api_url" >&2
            # Print the full response for debugging if it's not empty
            else
                 echo "Error: Could not determine default branch for $owner_repo. Received response:" >&2
                 echo "$repo_info" >&2
            fi
            exit 1
        fi
    else
        echo "Error: Unrecognized GitHub URL format: $url" >&2
        usage
        exit 1
    fi
else
    echo "Error: Unrecognized URL format: $url" >&2
    usage
    exit 1
fi

# If api_url was not set by an API URL input, construct it now
if [[ -z "$api_url" ]]; then
    api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
fi

# Now use the constructed api_url to fetch the commit details
commit_info=$(curl -s "$api_url")

# Extract the actual commit SHA and tree SHA from the response
fetched_commit_sha=$(echo "$commit_info" | jq -r '.sha')
tree_sha=$(echo "$commit_info" | jq -r '.commit.tree.sha')

# Check if jq extraction failed (e.g., commit not found)
if [[ "$fetched_commit_sha" == "null" || -z "$fetched_commit_sha" ]]; then
    echo "Error: Could not fetch commit details for $commit_ref on $owner_repo" >&2
    echo "API URL used: $api_url" >&2
    # Print the full response for debugging if it's not empty
    if [[ -n "$commit_info" ]]; then
        echo "Received response:" >&2
        echo "$commit_info" >&2
    fi
    exit 1
fi

# Construct the JSON output using a single jq command
# Pass all potential values and let jq decide whether to use commit_ref as branch or null
jq -n \
    --arg owner "$owner_repo" \
    --arg commit "$fetched_commit_sha" \
    --arg tree "$tree_sha" \
    --arg commit_ref_val "$commit_ref" \
    --argjson is_branch_flag "$is_branch" \
    '{owner: $owner, commit: $commit, tree: $tree, branch: (if $is_branch_flag == true then $commit_ref_val else null end)}'
