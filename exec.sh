#!/bin/sh

set -xe
ARGS=$@
stack exec -- idris --ibcsubdir tmp --codegen juvix $ARGS
