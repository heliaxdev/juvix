#!/bin/sh

set -xe
ARGS=$@
mkdir -p tmp
rm -f tmp/*.ibc
stack exec -- idris --ibcsubdir tmp --codegen juvix $ARGS
