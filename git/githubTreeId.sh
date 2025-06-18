#!/usr/bin/env bash
set -e

curl -s https://api.github.com/repos/balena-io/etcher/commits/aa6d526fea010d181f49dd81ae3bdaefb8d1938e | jq -r '.commit.tree.sha'
