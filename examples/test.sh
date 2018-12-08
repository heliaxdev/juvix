#!/bin/sh

set -xe

examples=$(ls *.idr)
for example in ${examples[@]}; do
  echo "Typechecking $example..."
  stack exec -- idris -p contrib --check $example
done
