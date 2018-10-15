#!/bin/sh

set -xe
mkdir -p tmp
rm -f tmp/*.ibc
ARGS=$@
stack exec -- idris --ibcsubdir tmp --codegen juvix $ARGS
