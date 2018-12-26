#!/bin/sh

set -e

files=$(find ./examples -type f -name "*.idr" | sort)

for file in ${files[@]}; do
  echo "Typechecking $file..."
  stack exec -- idris -p contrib --check $file
done

TMP=$(mktemp -d)

for file in ${files[@]}; do
  echo "Compiling $file..."
  ./exec.sh $file -o $TMP/test.tz
  test -f $TMP/test.tz
done

rm -rf $TMP
