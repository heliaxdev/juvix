#!/bin/sh

set -xe

TMP=$(mktemp -d)

for file in $(ls examples/*.idr); do
  ./exec.sh $file -o $TMP/test.tz
  test -f $TMP/test.tz
done

rm -rf $TMP
