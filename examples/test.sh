#!/bin/sh

set -e
examples=$(ls *.idr)
for example in ${examples[@]}; do
  echo "Typechecking $example..."
  stack exec -- idris --check $example
done
