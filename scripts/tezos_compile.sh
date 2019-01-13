#!/bin/sh

set -e

ARGS=$@
mkdir -p tmp
rm -f tmp/*.ibc

set -x

stack exec -- idris -p contrib --ibcsubdir tmp --codegen juvix $ARGS
