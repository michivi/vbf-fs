#!/usr/bin/env bash

set -e

SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

[ -f /etc/os-release ] && . /etc/os-release

OS_ID=${OS_ID:-".${ID}-${VERSION_ID}"}
SUFFIX="$([ ${#OS_ID} -gt 2 ] && echo -n "$OS_ID" || echo -n "")"

echo "Releasing project from '$SCRIPT_DIR'..."

OUTPUT_DIR=$SCRIPT_DIR/build

[ -d "$OUTPUT_DIR" ] && rm -rf $OUTPUT_DIR/* 2> /dev/null
mkdir -p $OUTPUT_DIR

stack $VBFFS_STACK_ARGS clean
stack $VBFFS_STACK_ARGS build --haddock --haddock-arguments "--odir=$OUTPUT_DIR/doc" --copy-bins --local-bin-path=$OUTPUT_DIR/bin/$MACHTYPE$SUFFIX
tar zcf $OUTPUT_DIR/bin-$MACHTYPE$SUFFIX.tar.gz -C $OUTPUT_DIR bin
tar zcf $OUTPUT_DIR/haddock.tar.gz -C $OUTPUT_DIR doc

stack $VBFFS_STACK_ARGS sdist --tar-dir $OUTPUT_DIR

echo "Released in $OUTPUT_DIR."
