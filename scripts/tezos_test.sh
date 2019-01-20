#!/bin/bash

set -e

files=$(find ./examples -type f -name "*.idr" | sort)
count=$(echo "$files" | wc -l)

echo "Found $count example contracts"

for file in ${files[@]}; do
  echo "Typechecking $file..."
  stack exec -- idris --noprelude -p tezos --check $file
done

for file in ${files[@]}; do
  echo "Compiling $file..."
  OUT=$(mktemp).tz
  ./scripts/tezos_compile.sh $file -o $OUT
  test -f $OUT
done

rm -rf $TMP
