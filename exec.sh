#!/bin/sh

set -xe
rm -f tmp/*.ibc
ARGS=$@
stack exec -- idris --noprelude --ibcsubdir tmp --codegen juvix $ARGS
