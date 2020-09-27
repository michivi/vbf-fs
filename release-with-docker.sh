#!/usr/bin/env bash

set -e

docker build docker/stack -t michivi/vbf-fs-stack:latest

eval "$(docker run --rm michivi/vbf-fs-stack:latest cat /etc/os-release)"

OS_ID=".${ID}-${VERSION_ID}" VBFFS_STACK_ARGS="--docker --no-nix" ./release.sh
