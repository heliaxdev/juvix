#!/bin/sh

set -xe
ARGS=$@
stack exec -- idris -O0 --noprelude --ibcsubdir tmp --codegen juvix $ARGS
