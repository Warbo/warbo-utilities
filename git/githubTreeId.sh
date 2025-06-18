#!/usr/bin/env bash
set -e

usage() {
    {
        for msg in "$@"
        do
            echo "$msg"
        done
        echo "Usage: $0 <github_url>"
        echo "Supported github urls:"
        echo "  https://api.github.com/repos/<owner>/<repo>/commits/<hash_or_branch>"
        echo "  https://github.com/<owner>/<repo>/commit/<hash>"
        echo "  https://github.com/<owner>/<repo>/tree/<branch>"
        echo "  https://github.com/<owner>/<repo> (fetches default branch)"
        exit 1
    } >&2
}

url="$1"
[[ -n "$url" ]] || usage

api_url=""
owner_repo=""
commit_ref="" # Can be hash or branch name, depending on is_branch
is_branch=false

if [[ "$url" =~ ^https://api\.github\.com/repos/([^/]+/[^/]+)/commits/([^/]+) ]]; then
    owner_repo=${BASH_REMATCH[1]}
    commit_ref=${BASH_REMATCH[2]}
elif [[ "$url" =~ ^https://github\.com/([^/]+/[^/]+) ]]; then
    owner_repo=${BASH_REMATCH[1]}

    if [[ "$url" =~ /commit/([^/]+) ]]; then
        commit_ref=${BASH_REMATCH[1]}
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
    elif [[ "$url" =~ /tree/([^/]+) ]]; then
        is_branch=true
        commit_ref=${BASH_REMATCH[1]}
        api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
    elif [[ "$url" =~ ^https://github\.com/([^/]+/[^/]+)$ ]]; then
        repo_api_url="https://api.github.com/repos/${owner_repo}"
        repo_info=$(curl -s "$repo_api_url")
        if default_branch=$(echo "$repo_info" | jq -re '.default_branch'); then
            commit_ref="$default_branch"
            api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
            is_branch=true
        else
            {
                echo "Error: Could not determine default branch for $owner_repo"
                if [[ -z "$repo_info" ]]; then
                    echo "Error: Received empty response from '$repo_api_url'"
                else
                    echo "Received response:"
                    echo "$repo_info"
                fi
                exit 1
            } >&2
        fi
    else
        usage "Error: Unrecognized GitHub URL format: $url"
    fi
else
    usage "Error: Unrecognized URL format: $url"
fi

[[ -n "$api_url" ]] || {
    api_url="https://api.github.com/repos/${owner_repo}/commits/${commit_ref}"
}
commit_info=$(curl -s "$api_url")
[[ -n "$commit_info" ]] || {
    echo "Error: Received empty response from '$api_url'"
    exit 1
} >&2

echo "$commit_info" | jq \
   --arg owner "$owner_repo" \
   --arg commit_ref_val "$commit_ref" \
   --argjson is_branch_flag "$is_branch" \
   '{
        owner: $owner,
        commit: .sha, # Access directly from input
        tree: .commit.tree.sha, # Access directly from input
        branch: (if $is_branch_flag == true then $commit_ref_val else null end)
    }'
