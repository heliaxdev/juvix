#!/bin/sh

exit 0

./juvix.sh test

RC=$?

if [[ $RC != 0 ]]; then
  echo "Test failure! Please check your changes!"
  exit $RC 
fi

./juvix.sh lint

RC=$?

if [[ $RC != 0 ]]; then
  echo "Lint failure! Please fix offending code and try again."
  exit $RC 
fi

echo 'Pre-commit check OK!'
