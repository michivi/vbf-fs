#!/usr/bin/env bash

set -e

OUTPUT_DIR=$PWD/build

rm -rf $OUTPUT_DIR 2> /dev/null
mkdir -p $OUTPUT_DIR

stack build --haddock --haddock-arguments "--odir=$OUTPUT_DIR/doc" --copy-bins --local-bin-path=$OUTPUT_DIR/bin
stack sdist --tar-dir $OUTPUT_DIR/sdist

echo "Released in $OUTPUT_DIR."
